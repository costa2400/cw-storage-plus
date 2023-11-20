# `cw-storage-plus`: Storage abstractions for CosmWasm

**Status: beta**

Status: Beta

Welcome to cw-storage-plus version v0.12, a cornerstone library in the CosmWasm ecosystem, renowned for its stability and robustness. 
This library serves as the backbone for numerous production-quality contracts, demonstrating exceptional reliability and power in real-world applications.
While not formally audited and with no liability assumed by Confio, cw-storage-plus stands as the preferred and mature choice for smart contract storage needs.

## Comprehensive Usage Overview

cw-storage-plus introduces two pivotal classes, Item and Map, as advanced abstractions over cosmwasm_std::Storage. 
These classes represent a paradigm shift in how storage mechanisms are conceptualized and utilized within the CosmWasm ecosystem.

## Item
Item is ingeniously crafted as a typed wrapper around a singular database key, enabling developers to interact with blockchain data without the complexities of raw byte handling. This model provides a seamless interface for data manipulation, fostering a development environment where safety and efficiency are paramount.

## Technical Insights:
- Type Safety: Item ensures robust type consistency, thereby reducing runtime errors and enhancing code clarity.
- Gas Efficiency: The use of const fn for Item instantiation signifies a shift towards compile-time constant definitions, leading to substantial gas savings and reduced operational costs.
- Singleton Transition: Developers accustomed to Singleton will notice that Item doesn't encapsulate Storage, streamlining the read/write process and eliminating the need for separate object variants.

Example Usage:

```rust
#[derive(Serialize, Deserialize, PartialEq, Debug)]
struct Config {
    pub owner: String,
    pub max_tokens: i32,
}

// note const constructor rather than 2 functions with Singleton
const CONFIG: Item<Config> = Item::new("config");

fn demo() -> StdResult<()> {
    let mut store = MockStorage::new();

    // may_load returns Option<T>, so None if data is missing
    // load returns T and Err(StdError::NotFound{}) if data is missing
    let empty = CONFIG.may_load(&store)?;
    assert_eq!(None, empty);
    let cfg = Config {
        owner: "admin".to_string(),
        max_tokens: 1234,
    };
    CONFIG.save(&mut store, &cfg)?;
    let loaded = CONFIG.load(&store)?;
    assert_eq!(cfg, loaded);

    // update an item with a closure (includes read and write)
    // returns the newly saved value
    let output = CONFIG.update(&mut store, |mut c| -> StdResult<_> {
        c.max_tokens *= 2;
        Ok(c)
    })?;
    assert_eq!(2468, output.max_tokens);

    // you can error in an update and nothing is saved
    let failed = CONFIG.update(&mut store, |_| -> StdResult<_> {
        Err(StdError::generic_err("failure mode"))
    });
    assert!(failed.is_err());

    // loading data will show the first update was saved
    let loaded = CONFIG.load(&store)?;
    let expected = Config {
        owner: "admin".to_string(),
        max_tokens: 2468,
    };
    assert_eq!(expected, loaded);

    // we can remove data as well
    CONFIG.remove(&mut store);
    let empty = CONFIG.may_load(&store)?;
    assert_eq!(None, empty);

    Ok(())
}
```

## Map

Map in cw-storage-plus, though a bit more complex than Item, is still straightforward to use. 
Conceptually, it's akin to a storage-backed BTreeMap, allowing you to perform key-value lookups with typed values. 
It supports a variety of key types:

Simple Binary Keys: For example, &[u8], allowing basic key-value associations.
Composite Keys (Tuples): Such as (owner, spender), enabling more nuanced data relationships like storing allowances based on account owner and spender.
One of the exceptional features of Map is its capability for iteration. This is a powerful tool not typically found in Ethereum's smart contract ecosystem. With Map, you can:

List All Items: Either all the items stored in the Map or only a specific subset.
Efficient Pagination: Iteratively access these items, starting from where the last query ended, and do so with minimal gas costs. This feature is enabled by default thanks to the iterator feature in cw-storage-plus and cosmwasm-std.
For developers transitioning from using Bucket, the major change in Map is its internal handling of Storage. Unlike Bucket, Map doesn’t encapsulate Storage inside. This means:

No Need for Read and Write Variants: Simplifying the object model to just one type.
Use of const fn for Initialization: Allowing Map to be defined as a global compile-time constant. This not only saves gas but also simplifies code, as you don’t need to construct a Map instance each time it's used.
Improved Composite Indexes and Range Interface: The use of tuples for keys is more ergonomic and expressive, and the range interface for queries has been improved, making data handling more efficient.


Here is an example with normal (simple) keys:

```rust
#[derive(Serialize, Deserialize, PartialEq, Debug, Clone)]
struct Data {
    pub name: String,
    pub age: i32,
}

const PEOPLE: Map<&str, Data> = Map::new("people");

fn demo() -> StdResult<()> {
    let mut store = MockStorage::new();
    let data = Data {
        name: "John".to_string(),
        age: 32,
    };

    // load and save with extra key argument
    let empty = PEOPLE.may_load(&store, "john")?;
    assert_eq!(None, empty);
    PEOPLE.save(&mut store, "john", &data)?;
    let loaded = PEOPLE.load(&store, "john")?;
    assert_eq!(data, loaded);

    // nothing on another key
    let missing = PEOPLE.may_load(&store, "jack")?;
    assert_eq!(None, missing);

    // update function for new or existing keys
    let birthday = |d: Option<Data>| -> StdResult<Data> {
        match d {
            Some(one) => Ok(Data {
                name: one.name,
                age: one.age + 1,
            }),
            None => Ok(Data {
                name: "Newborn".to_string(),
                age: 0,
            }),
        }
    };

    let old_john = PEOPLE.update(&mut store, "john", birthday)?;
    assert_eq!(33, old_john.age);
    assert_eq!("John", old_john.name.as_str());

    let new_jack = PEOPLE.update(&mut store, "jack", birthday)?;
    assert_eq!(0, new_jack.age);
    assert_eq!("Newborn", new_jack.name.as_str());

    // update also changes the store
    assert_eq!(old_john, PEOPLE.load(&store, "john")?);
    assert_eq!(new_jack, PEOPLE.load(&store, "jack")?);

    // removing leaves us empty
    PEOPLE.remove(&mut store, "john");
    let empty = PEOPLE.may_load(&store, "john")?;
    assert_eq!(None, empty);

    Ok(())
}
```

## Key Types in 'cw-storage-plus'

In 'cw-storage-plus', the versatility of Map is greatly enhanced by its ability to utilize a variety of key types, as determined by the 'PrimaryKey' trait. This flexibility allows developers to tailor data structures according to the specific requirements of their smart contracts.

## Implementations of PrimaryKey
-Binary Slices (&[u8]): Fundamental for representing raw binary data, suitable for compact and efficient storage.
-String Slices (&str): Ideal for human-readable keys, commonly used for identifiers that are textual in nature.
-Byte Vectors (Vec<u8>): Offer flexibility in key size, useful for dynamically constructed keys.
-Owned Strings (String): Similar to string slices but for scenarios where key ownership and mutability are required.
-Blockchain Addresses (Addr): Specifically optimized for blockchain address formats, ensuring validity and consistency in address-based keys.
-Fixed-Size Byte Arrays ([u8; N]): Useful for keys with a fixed size, providing a balance between performance and predictability.
-Composite Keys (Tuples): Enable complex data relationships and queries by combining multiple key components. Particularly powerful in scenarios where data is categorized or relational.

## Address Keys and Validation
- Using 'Addr' over Strings: When keys represent blockchain addresses, using '&Addr' instead of string types is advisable. This ensures that the address is valid and correctly formatted, preventing errors and inconsistencies.
- Address Validation: Utilize 'addr_validate' for any address input, converting it into an 'Addr' type. This validation is crucial to ensure that the addresses used as keys are legitimate and not arbitrary or malformed strings.
  
## Efficiency Considerations
- Reference vs. Value: Favoring references (like &Addr) over owned values (like Addr) can significantly reduce memory usage and processing time, especially in read-heavy scenarios where cloning can be avoided.
- Optimal Key Selection: The choice of key type should be guided by the nature of the data and the operations to be performed. For instance, using &[u8] or &str for simple lookups, Vec<u8> for variable-length keys, or Addr for address-based data.
## Practical Implications
Understanding these key types and their implementations is vital for creating efficient and robust data structures in 'cw-storage-plus'. The choice of key type affects not only the performance of data operations but also the ease of development and maintenance of smart contracts. Developers should carefully consider their data modeling needs and choose key types that align with their contract's functionality and performance requirements.



### Composite Keys

There are times when we want to use multiple items as a key. For example, when
storing allowances based on account owner and spender. We could try to manually
concatenate them before calling, but that can lead to overlap, and is a bit
low-level for us. Also, by explicitly separating the keys, we can easily provide
helpers to do range queries over a prefix, such as "show me all allowances for
one owner" (first part of the composite key). Just like you'd expect from your
favorite database.

Here's how we use it with composite keys. Just define a tuple as a key and use that
everywhere you used a single key above.

```rust
// Note the tuple for primary key. We support one slice, or a 2 or 3-tuple.
// Adding longer tuples is possible, but unlikely to be needed.
const ALLOWANCE: Map<(&str, &str), u64> = Map::new("allow");

fn demo() -> StdResult<()> {
    let mut store = MockStorage::new();

    // save and load on a composite key
    let empty = ALLOWANCE.may_load(&store, ("owner", "spender"))?;
    assert_eq!(None, empty);
    ALLOWANCE.save(&mut store, ("owner", "spender"), &777)?;
    let loaded = ALLOWANCE.load(&store, ("owner", "spender"))?;
    assert_eq!(777, loaded);

    // doesn't appear under other key (even if a concat would be the same)
    let different = ALLOWANCE.may_load(&store, ("owners", "pender")).unwrap();
    assert_eq!(None, different);

    // simple update
    ALLOWANCE.update(&mut store, ("owner", "spender"), |v| {
        Ok(v.unwrap_or_default() + 222)
    })?;
    let loaded = ALLOWANCE.load(&store, ("owner", "spender"))?;
    assert_eq!(999, loaded);

    Ok(())
}
```

### Path

Under the scenes, we create a `Path` from the `Map` when accessing a key.
`PEOPLE.load(&store, "jack") == PEOPLE.key("jack").load()`.
`Map.key()` returns a `Path`, which has the same interface as `Item`,
re-using the calculated path to this key.

For simple keys, this is just a bit less typing and a bit less gas if you
use the same key for many calls. However, for composite keys, like
`("owner", "spender")` it is **much** less typing. And highly recommended anywhere
you will use a composite key even twice:

```rust
#[derive(Serialize, Deserialize, PartialEq, Debug, Clone)]
struct Data {
    pub name: String,
    pub age: i32,
}

const PEOPLE: Map<&str, Data> = Map::new("people");
const ALLOWANCE: Map<(&str, &str), u64> = Map::new("allow");

fn demo() -> StdResult<()> {
    let mut store = MockStorage::new();
    let data = Data {
        name: "John".to_string(),
        age: 32,
    };

    // create a Path one time to use below
    let john = PEOPLE.key("john");

    // Use this just like an Item above
    let empty = john.may_load(&store)?;
    assert_eq!(None, empty);
    john.save(&mut store, &data)?;
    let loaded = john.load(&store)?;
    assert_eq!(data, loaded);
    john.remove(&mut store);
    let empty = john.may_load(&store)?;
    assert_eq!(None, empty);

    // Same for composite keys, just use both parts in `key()`.
    // Notice how much less verbose than the above example.
    let allow = ALLOWANCE.key(("owner", "spender"));
    allow.save(&mut store, &1234)?;
    let loaded = allow.load(&store)?;
    assert_eq!(1234, loaded);
    allow.update(&mut store, |x| Ok(x.unwrap_or_default() * 2))?;
    let loaded = allow.load(&store)?;
    assert_eq!(2468, loaded);

    Ok(())
}
```

### Prefix

In addition to getting one particular item out of a map, we can iterate over the map
(or a subset of the map). This let us answer questions like "show me all tokens",
and we provide some nice [`Bound`](#bound) helpers to easily allow pagination or custom ranges.

The general format is to get a `Prefix` by calling `map.prefix(k)`, where `k` is exactly
one less item than the normal key (If `map.key()` took `(&[u8], &[u8])`, then `map.prefix()` takes `&[u8]`.
If `map.key()` took `&[u8]`, `map.prefix()` takes `()`). Once we have a prefix space, we can iterate
over all items with `range(store, min, max, order)`. It supports `Order::Ascending` or `Order::Descending`.
`min` is the lower bound and `max` is the higher bound.

If the `min` and `max` bounds are `None`, `range` will return all items under the prefix. You can use `.take(n)` to
limit the results to `n` items and start doing pagination. You can also set the `min` bound to
eg. `Bound::exclusive(last_value)` to start iterating over all items *after* the last value. Combined with
`take`, we easily have pagination support. You can also use `Bound::inclusive(x)` when you want to include any
perfect matches.

### Bound

`Bound` is a helper to build type-safe bounds on the keys or sub-keys you want to iterate over.
It also supports a raw (`Vec<u8>`) bounds specification, for the cases you don't want or can't use typed bounds.

```rust
#[derive(Clone, Debug)]
pub enum Bound<'a, K: PrimaryKey<'a>> {
  Inclusive((K, PhantomData<&'a bool>)),
  Exclusive((K, PhantomData<&'a bool>)),
  InclusiveRaw(Vec<u8>),
  ExclusiveRaw(Vec<u8>),
}
```

To better understand the API, please check the following example:
```rust
#[derive(Serialize, Deserialize, PartialEq, Debug, Clone)]
struct Data {
    pub name: String,
    pub age: i32,
}

const PEOPLE: Map<&str, Data> = Map::new("people");
const ALLOWANCE: Map<(&str, &str), u64> = Map::new("allow");

fn demo() -> StdResult<()> {
    let mut store = MockStorage::new();

    // save and load on two keys
    let data = Data { name: "John".to_string(), age: 32 };
    PEOPLE.save(&mut store, "john", &data)?;
    let data2 = Data { name: "Jim".to_string(), age: 44 };
    PEOPLE.save(&mut store, "jim", &data2)?;

    // iterate over them all
    let all: StdResult<Vec<_>> = PEOPLE
        .range(&store, None, None, Order::Ascending)
        .collect();
    assert_eq!(
        all?,
        vec![("jim".to_vec(), data2), ("john".to_vec(), data.clone())]
    );

    // or just show what is after jim
    let all: StdResult<Vec<_>> = PEOPLE
        .range(
            &store,
            Some(Bound::exclusive("jim")),
            None,
            Order::Ascending,
        )
        .collect();
    assert_eq!(all?, vec![("john".to_vec(), data)]);

    // save and load on three keys, one under different owner
    ALLOWANCE.save(&mut store, ("owner", "spender"), &1000)?;
    ALLOWANCE.save(&mut store, ("owner", "spender2"), &3000)?;
    ALLOWANCE.save(&mut store, ("owner2", "spender"), &5000)?;

    // get all under one key
    let all: StdResult<Vec<_>> = ALLOWANCE
        .prefix("owner")
        .range(&store, None, None, Order::Ascending)
        .collect();
    assert_eq!(
        all?,
        vec![("spender".to_vec(), 1000), ("spender2".to_vec(), 3000)]
    );

    // Or ranges between two items (even reverse)
    let all: StdResult<Vec<_>> = ALLOWANCE
        .prefix("owner")
        .range(
            &store,
            Some(Bound::exclusive("spender")),
            Some(Bound::inclusive("spender2")),
            Order::Descending,
        )
        .collect();
    assert_eq!(all?, vec![("spender2".to_vec(), 3000)]);

    Ok(())
}
```

**NB**: For properly defining and using type-safe bounds over a `MultiIndex`, see [Type-safe bounds over `MultiIndex`](#type-safe-bounds-over-multiindex),
below.

## IndexedMap

Let's see one example of `IndexedMap` definition and usage, originally taken from the `cw721-base` contract.

### Definition

```rust
pub struct TokenIndexes<'a> {
  pub owner: MultiIndex<'a, Addr, TokenInfo, String>,
}

impl<'a> IndexList<TokenInfo> for TokenIndexes<'a> {
  fn get_indexes(&'_ self) -> Box<dyn Iterator<Item = &'_ dyn Index<TokenInfo>> + '_> {
    let v: Vec<&dyn Index<TokenInfo>> = vec![&self.owner];
    Box::new(v.into_iter())
  }
}

pub fn tokens<'a>() -> IndexedMap<'a, &'a str, TokenInfo, TokenIndexes<'a>> {
  let indexes = TokenIndexes {
    owner: MultiIndex::new(
      |d: &TokenInfo| d.owner.clone(),
      "tokens",
      "tokens__owner",
    ),
  };
  IndexedMap::new("tokens", indexes)
}
```

Let's discuss this piece by piece:
```rust
pub struct TokenIndexes<'a> {
  pub owner: MultiIndex<'a, Addr, TokenInfo, String>,
}
```

These are the index definitions. Here there's only one index, called `owner`. There could be more, as public
members of the `TokenIndexes` struct.
We see that the `owner` index is a `MultiIndex`. A multi-index can have repeated values as keys. The primary key is
used internally as the last element of the multi-index key, to disambiguate repeated index values.
Like the name implies, this is an index over tokens, by owner. Given that an owner can have multiple tokens,
we need a `MultiIndex` to be able to list / iterate over all the tokens he has.

The `TokenInfo` data will originally be stored by `token_id` (which is a string value).
You can see this in the token creation code:
```rust
    tokens().update(deps.storage, &msg.token_id, |old| match old {
        Some(_) => Err(ContractError::Claimed {}),
        None => Ok(token),
    })?;
```
(Incidentally, this is using `update` instead of `save`, to avoid overwriting an already existing token).

Given that `token_id` is a string value, we specify `String` as the last argument of the `MultiIndex` definition.
That way, the deserialization of the primary key will be done to the right type (an owned string).

**NB**: In the particular case of a `MultiIndex`, and with the latest implementation of type-safe bounds, the definition of
this last type parameter is crucial, for properly using type-safe bounds.
See [Type-safe bounds over `MultiIndex`](#type-safe-bounds-over-multiindex), below.

Then, this `TokenInfo` data will be indexed by token `owner` (which is an `Addr`). So that we can list all the tokens
an owner has. That's why the `owner` index key is `Addr`.

Other important thing here is that the key (and its components, in the case of a composite key) must implement
the `PrimaryKey` trait. You can see that `Addr` does implement `PrimaryKey`:

```rust
impl<'a> PrimaryKey<'a> for Addr {
  type Prefix = ();
  type SubPrefix = ();
  type Suffix = Self;
  type SuperSuffix = Self;

  fn key(&self) -> Vec<Key> {
    // this is simple, we don't add more prefixes
    vec![Key::Ref(self.as_bytes())]
  }
}
```

---

We can now see how it all works, taking a look at the remaining code:

```rust
impl<'a> IndexList<TokenInfo> for TokenIndexes<'a> {
    fn get_indexes(&'_ self) -> Box<dyn Iterator<Item = &'_ dyn Index<TokenInfo>> + '_> {
        let v: Vec<&dyn Index<TokenInfo>> = vec![&self.owner];
        Box::new(v.into_iter())
    }
}
```

This implements the `IndexList` trait for `TokenIndexes`.

**NB**: this code is more or less boiler-plate, and needed for the internals. Do not try to customize this;
just return a list of all indexes.
Implementing this trait serves two purposes (which are really one and the same): it allows the indexes
to be queried through `get_indexes`, and, it allows `TokenIndexes` to be treated as an `IndexList`. So that
it can be passed as a parameter during `IndexedMap` construction, below:

```rust
pub fn tokens<'a>() -> IndexedMap<'a, &'a str, TokenInfo, TokenIndexes<'a>> {
    let indexes = TokenIndexes {
        owner: MultiIndex::new(
            |d: &TokenInfo| d.owner.clone(),
            "tokens",
            "tokens__owner",
        ),
    };
    IndexedMap::new("tokens", indexes)
}
```

Here `tokens()` is just a helper function, that simplifies the `IndexedMap` construction for us. First the
index (es) is (are) created, and then, the `IndexedMap` is created and returned.

During index creation, we must supply an index function per index
```rust
        owner: MultiIndex::new(|d: &TokenInfo| d.owner.clone(),
```
which is the one that will take the value of the original map and create the index key from it.
Of course, this requires that the elements required for the index key are present in the value.
Besides the index function, we must also supply the namespace of the pk, and the one for the new index.

---

After that, we just create and return the `IndexedMap`:

```rust
    IndexedMap::new("tokens", indexes)
```

Here of course, the namespace of the pk must match the one used during index(es) creation. And, we pass our
`TokenIndexes` (as an `IndexList`-type parameter) as second argument. Connecting in this way the underlying `Map`
for the pk, with the defined indexes.

So, `IndexedMap` (and the other `Indexed*` types) is just a wrapper / extension around `Map`, that provides
a number of index functions and namespaces to create indexes over the original `Map` data. It also implements
calling these index functions during value storage / update / removal, so that you can forget about it,
and just use the indexed data.

### Usage

An example of use, where `owner` is a `String` value passed as a parameter, and `start_after` and `limit` optionally
define the pagination range:

Notice this uses `prefix()`, explained above in the `Map` section.

```rust
    let limit = limit.unwrap_or(DEFAULT_LIMIT).min(MAX_LIMIT) as usize;
    let start = start_after.map(Bound::exclusive);
    let owner_addr = deps.api.addr_validate(&owner)?;

    let res: Result<Vec<_>, _> = tokens()
        .idx
        .owner
        .prefix(owner_addr)
        .range(deps.storage, start, None, Order::Ascending)
        .take(limit)
        .collect();
    let tokens = res?;
```
Now `tokens` contains `(token_id, TokenInfo)` pairs for the given `owner`.
The pk values are `Vec<u8>` in the case of `range_raw()`, but will be deserialized to the proper type using
`range()`; provided that the pk deserialization type (`String`, in this case) is correctly specified
in the `MultiIndex` definition (see [Index keys deserialization](#index-keys-deserialization),
below).

Another example that is similar, but returning only the (raw) `token_id`s, using the `keys_raw()` method:
```rust
    let pks: Vec<_> = tokens()
        .idx
        .owner
        .prefix(owner_addr)
        .keys_raw(
            deps.storage,
            start,
            None,
            Order::Ascending,
        )
        .take(limit)
        .collect();
```
Now `pks` contains `token_id` values (as raw `Vec<u8>`s) for the given `owner`. By using `keys` instead,
a deserialized key can be obtained, as detailed in the next section.

### Index keys deserialization

For `UniqueIndex` and `MultiIndex`, the primary key (`PK`) type needs to be specified, in order to deserialize
the primary key to it.
This `PK` type specification is also important for `MultiIndex` type-safe bounds, as the primary key
is part of the multi-index key. See next section, [Type-safe bounds over MultiIndex](#type-safe-bounds-over-multiindex).

**NB**: This specification is still a manual (and therefore error-prone) process / setup, that will (if possible)
be automated in the future (https://github.com/CosmWasm/cw-plus/issues/531).

### Type-safe bounds over MultiIndex

In the particular case of `MultiIndex`, the primary key (`PK`) type parameter also defines the type of the (partial) bounds over
the index key (the part that corresponds to the primary key, that is).
So, to correctly use type-safe bounds over multi-indexes ranges, it is fundamental for this `PK` type
to be correctly defined, so that it matches the primary key type, or its (typically owned) deserialization variant.

## Deque

The usage of a [`Deque`](./src/deque.rs) is pretty straight-forward.
Conceptually it works like a storage-backed version of Rust std's `Deque` and can be used as a queue or stack.
It allows you to push and pop elements on both ends and also read the first or last element without mutating the deque.
You can also read a specific index directly.

Example Usage:

```rust
#[derive(Serialize, Deserialize, PartialEq, Debug, Clone)]
struct Data {
    pub name: String,
    pub age: i32,
}

const DATA: Deque<Data> = Deque::new("data");

fn demo() -> StdResult<()> {
    let mut store = MockStorage::new();

    // read methods return a wrapped Option<T>, so None if the deque is empty
    let empty = DATA.front(&store)?;
    assert_eq!(None, empty);

    // some example entries
    let p1 = Data {
        name: "admin".to_string(),
        age: 1234,
    };
    let p2 = Data {
        name: "user".to_string(),
        age: 123,
    };

    // use it like a queue by pushing and popping at opposite ends
    DATA.push_back(&mut store, &p1)?;
    DATA.push_back(&mut store, &p2)?;

    let admin = DATA.pop_front(&mut store)?;
    assert_eq!(admin.as_ref(), Some(&p1));
    let user = DATA.pop_front(&mut store)?;
    assert_eq!(user.as_ref(), Some(&p2));

    // or push and pop at the same end to use it as a stack
    DATA.push_back(&mut store, &p1)?;
    DATA.push_back(&mut store, &p2)?;

    let user = DATA.pop_back(&mut store)?;
    assert_eq!(user.as_ref(), Some(&p2));
    let admin = DATA.pop_back(&mut store)?;
    assert_eq!(admin.as_ref(), Some(&p1));

    // you can also iterate over it
    DATA.push_front(&mut store, &p1)?;
    DATA.push_front(&mut store, &p2)?;

    let all: StdResult<Vec<_>> = DATA.iter(&store)?.collect();
    assert_eq!(all?, [p2, p1]);

    // or access an index directly
    assert_eq!(DATA.get(&store, 0)?, Some(p2));
    assert_eq!(DATA.get(&store, 1)?, Some(p1));
    assert_eq!(DATA.get(&store, 3)?, None);

    Ok(())
}
```
