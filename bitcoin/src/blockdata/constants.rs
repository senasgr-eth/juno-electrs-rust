// SPDX-License-Identifier: CC0-1.0

//! Blockdata constants.
//!
//! This module provides various constants relating to the blockchain and
//! consensus code. In particular, it defines the genesis block and its
//! single transaction.
//!

use core::default::Default;

use hashes::{sha256d, Hash};
use hex_lit::hex;
use internals::impl_array_newtype;

use crate::blockdata::block::{self, Block};
use crate::blockdata::locktime::absolute;
use crate::blockdata::opcodes::all::*;
use crate::blockdata::script;
use crate::blockdata::transaction::{self, OutPoint, Sequence, Transaction, TxIn, TxOut};
use crate::blockdata::witness::Witness;
use crate::internal_macros::impl_bytes_newtype;
use crate::network::Network;
use crate::pow::CompactTarget;
use crate::Amount;

/// How many seconds between blocks we expect on average (Junocash POST_BLOSSOM).
pub const TARGET_BLOCK_SPACING: u32 = 75;
/// How many blocks between diffchanges.
pub const DIFFCHANGE_INTERVAL: u32 = 1152;
/// How much time on average should occur between diffchanges (24 hours).
pub const DIFFCHANGE_TIMESPAN: u32 = 24 * 60 * 60;

#[deprecated(since = "0.31.0", note = "Use Weight::MAX_BLOCK instead")]
/// The maximum allowed weight for a block, see BIP 141 (network rule).
pub const MAX_BLOCK_WEIGHT: u32 = 4_000_000;

#[deprecated(since = "0.31.0", note = "Use Weight::MIN_TRANSACTION instead")]
/// The minimum transaction weight for a valid serialized transaction.
pub const MIN_TRANSACTION_WEIGHT: u32 = 4 * 60;

/// The factor that non-witness serialization data is multiplied by during weight calculation.
pub const WITNESS_SCALE_FACTOR: usize = 4;
/// The maximum allowed number of signature check operations in a block.
pub const MAX_BLOCK_SIGOPS_COST: i64 = 80_000;
/// Mainnet (Junocash) pubkey address prefix.
pub const PUBKEY_ADDRESS_PREFIX_MAIN: u8 = 0x1C;
/// Mainnet (Junocash) script address prefix.
pub const SCRIPT_ADDRESS_PREFIX_MAIN: u8 = 0x1C;
/// Test (signet, regtest) pubkey address prefix.
pub const PUBKEY_ADDRESS_PREFIX_TEST: u8 = 0x1D;
/// Test (tesnet, signet, regtest) script address prefix.
pub const SCRIPT_ADDRESS_PREFIX_TEST: u8 = 0x1C;
// Regtest pubkey address prefix.
pub const PUBKEY_ADDRESS_PREFIX_REGTEST: u8 = 111; // 0x6f
/// The maximum allowed script size.
pub const MAX_SCRIPT_ELEMENT_SIZE: usize = 520;
/// How may blocks between halvings (Junocash uses 840000 pre-blossom).
pub const SUBSIDY_HALVING_INTERVAL: u32 = 840_000;
/// Maximum allowed value for an integer in Script.
pub const MAX_SCRIPTNUM_VALUE: u32 = 0x80000000; // 2^31
/// Number of blocks needed for an output from a coinbase transaction to be spendable.
pub const COINBASE_MATURITY: u32 = 100;

/// Constructs and returns the coinbase (and only) transaction of the Junocash genesis block.
fn bitcoin_genesis_tx() -> Transaction {
    // Base
    let mut ret = Transaction {
        version: transaction::Version::ONE,
        lock_time: absolute::LockTime::ZERO,
        input: vec![],
        output: vec![],
    };

    // Inputs
    let in_script = script::Builder::new()
        .push_int(520617983)
        .push_int_non_minimal(4)
        .push_slice(b"b7c923155000000000007ca8a49b1fc30e20bd9d5a3ca9a092af920f2f27b0c3")
        .into_script();
    ret.input.push(TxIn {
        previous_output: OutPoint::null(),
        script_sig: in_script,
        sequence: Sequence::MAX,
        witness: Witness::default(),
    });

    // Outputs
    let script_bytes = hex!("04678afdb0fe5548271967f1a67130b7105cd6a828e03909a67962e0ea1f61deb649f6bc3f4cef38c4f35504e51ec112de5c384df7ba0b8d578a4c702b6bf11d5f");
    let out_script =
        script::Builder::new().push_slice(script_bytes).push_opcode(OP_CHECKSIG).into_script();
    ret.output.push(TxOut { value: Amount::from_sat(0), script_pubkey: out_script });

    // end
    ret
}

/// Constructs and returns the genesis block.
pub fn genesis_block(network: Network) -> Block {
    let txdata = vec![bitcoin_genesis_tx()];
    let hash: sha256d::Hash = sha256d::Hash::from_slice(&hex!("672c43c131c7926fa7685cc000c9dc0450f7e803886b5c34445819686f8e53f3")).unwrap();
    let merkle_root = hash.into();
    match network {
        Network::Bitcoin => Block {
            header: block::Header {
                version: block::Version::from_consensus(4),
                prev_blockhash: Hash::all_zeros(),
                merkle_root,
                time: 1763197809,
                bits: CompactTarget::from_consensus(0x2000ffff),
                nonce: 0x1398,
                aux_data: None,
            },
            txdata,
        },
        Network::Testnet => Block {
            header: block::Header {
                version: block::Version::from_consensus(4),
                prev_blockhash: Hash::all_zeros(),
                merkle_root,
                time: 1763197808,
                bits: CompactTarget::from_consensus(0x2000ffff),
                nonce: 0xc0,
                aux_data: None,
            },
            txdata,
        },
        Network::Signet => Block {
            header: block::Header {
                version: block::Version::from_consensus(4),
                prev_blockhash: Hash::all_zeros(),
                merkle_root,
                time: 1763197808,
                bits: CompactTarget::from_consensus(0x2000ffff),
                nonce: 0xc0,
                aux_data: None,
            },
            txdata,
        },
        Network::Regtest => Block {
            header: block::Header {
                version: block::Version::from_consensus(4),
                prev_blockhash: Hash::all_zeros(),
                merkle_root,
                time: 1763197807,
                bits: CompactTarget::from_consensus(0x200f0f0f),
                nonce: 0x36,
                aux_data: None,
            },
            txdata,
        },
    }
}

/// The uniquely identifying hash of the target blockchain.
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ChainHash([u8; 32]);
impl_array_newtype!(ChainHash, u8, 32);
impl_bytes_newtype!(ChainHash, 32);

impl ChainHash {
    /// `ChainHash` for Junocash mainnet.
    /// Genesis hash: 0x0091ff2592b34a24eb014637f76c5ee416ce7a6928e8940f96e78954351d70bc
    pub const BITCOIN: Self = Self([0xbc, 0x70, 0x1d, 0x35, 0x54, 0x89, 0xe7, 0x96, 0x0f, 0x94, 0xe8, 0x28, 0x69, 0x7a, 0xce, 0x16, 0xe4, 0x5e, 0x6c, 0xf7, 0x37, 0x46, 0x01, 0xeb, 0x24, 0x4a, 0xb3, 0x92, 0x25, 0xff, 0x91, 0x00]);
    /// `ChainHash` for Junocash testnet.
    /// Genesis hash: 0x009a83c6bd95d1f0548fe4c5f6555c785e9c456ca33f58c2d7755c2bdd1e842f
    pub const TESTNET: Self = Self([0x2f, 0x84, 0x1e, 0xdd, 0x2b, 0x5c, 0x75, 0xd7, 0xc2, 0x58, 0x3f, 0xa3, 0x6c, 0x45, 0x9c, 0x5e, 0x78, 0x5c, 0x55, 0xf6, 0xc5, 0xe4, 0x8f, 0x54, 0xf0, 0xd1, 0x95, 0xbd, 0xc6, 0x83, 0x9a, 0x00]);
    /// `ChainHash` for Junocash signet (using testnet values).
    pub const SIGNET: Self = Self([0x2f, 0x84, 0x1e, 0xdd, 0x2b, 0x5c, 0x75, 0xd7, 0xc2, 0x58, 0x3f, 0xa3, 0x6c, 0x45, 0x9c, 0x5e, 0x78, 0x5c, 0x55, 0xf6, 0xc5, 0xe4, 0x8f, 0x54, 0xf0, 0xd1, 0x95, 0xbd, 0xc6, 0x83, 0x9a, 0x00]);
    /// `ChainHash` for Junocash regtest.
    /// Genesis hash: 0x02a19528ff5e8241dc7601cf7f54a74d26e0f2acc393a7ac964d055e6d1925db
    pub const REGTEST: Self = Self([0xdb, 0x25, 0x19, 0x6d, 0x5e, 0x05, 0x4d, 0x96, 0xac, 0xa7, 0x93, 0xc3, 0xac, 0xf2, 0xe0, 0x26, 0x4d, 0xa7, 0x54, 0x7f, 0xcf, 0x01, 0x76, 0xdc, 0x41, 0x82, 0x5e, 0xff, 0x28, 0x95, 0xa1, 0x02]);

    /// Returns the hash of the `network` genesis block for use as a chain hash.
    ///
    /// See [BOLT 0](https://github.com/lightning/bolts/blob/ffeece3dab1c52efdb9b53ae476539320fa44938/00-introduction.md#chain_hash)
    /// for specification.
    pub const fn using_genesis_block(network: Network) -> Self {
        let hashes = [Self::BITCOIN, Self::TESTNET, Self::SIGNET, Self::REGTEST];
        hashes[network as usize]
    }

    /// Converts genesis block hash into `ChainHash`.
    pub fn from_genesis_block_hash(block_hash: crate::BlockHash) -> Self {
        ChainHash(block_hash.to_byte_array())
    }
}

#[cfg(test)]
mod test {
    use core::str::FromStr;

    use hex::test_hex_unwrap as hex;

    use super::*;
    use crate::blockdata::locktime::absolute;
    use crate::blockdata::transaction;
    use crate::consensus::encode::serialize;
    use crate::network::Network;

    #[test]
    fn bitcoin_genesis_first_transaction() {
        let gen = bitcoin_genesis_tx();

        assert_eq!(gen.version, transaction::Version::ONE);
        assert_eq!(gen.input.len(), 1);
        assert_eq!(gen.input[0].previous_output.txid, Hash::all_zeros());
        assert_eq!(gen.input[0].previous_output.vout, 0xFFFFFFFF);
        assert_eq!(serialize(&gen.input[0].script_sig),
                   hex!("4d04ffff001d0104455468652054696d65732030332f4a616e2f32303039204368616e63656c6c6f72206f6e206272696e6b206f66207365636f6e64206261696c6f757420666f722062616e6b73"));

        assert_eq!(gen.input[0].sequence, Sequence::MAX);
        assert_eq!(gen.output.len(), 1);
        assert_eq!(serialize(&gen.output[0].script_pubkey),
                   hex!("434104678afdb0fe5548271967f1a67130b7105cd6a828e03909a67962e0ea1f61deb649f6bc3f4cef38c4f35504e51ec112de5c384df7ba0b8d578a4c702b6bf11d5fac"));
        assert_eq!(gen.output[0].value, Amount::from_str("50 BTC").unwrap());
        assert_eq!(gen.lock_time, absolute::LockTime::ZERO);

        assert_eq!(
            gen.wtxid().to_string(),
            "4a5e1e4baab89f3a32518a88c31bc87f618f76673e2cc77ab2127b7afdeda33b"
        );
    }

    #[test]
    fn bitcoin_genesis_full_block() {
        let gen = genesis_block(Network::Bitcoin);

        assert_eq!(gen.header.version, block::Version::ONE);
        assert_eq!(gen.header.prev_blockhash, Hash::all_zeros());
        assert_eq!(
            gen.header.merkle_root.to_string(),
            "71af8a6b906ecef9cf9cb05a593639f6bd2db7cefb9b2ceaed9065b97b01fa35"
        );

        assert_eq!(gen.header.time, 1367394064);
        assert_eq!(gen.header.bits, CompactTarget::from_consensus(0x1e0ffff0));
        assert_eq!(gen.header.nonce, 112158625);
        assert_eq!(
            gen.header.block_hash().to_string(),
            "a2effa738145e377e08a61d76179c21703e13e48910b30a2a87f0dfe794b64c6"
        );
    }

    #[test]
    fn testnet_genesis_full_block() {
        let gen = genesis_block(Network::Testnet);
        assert_eq!(gen.header.version, block::Version::ONE);
        assert_eq!(gen.header.prev_blockhash, Hash::all_zeros());
        assert_eq!(
            gen.header.merkle_root.to_string(),
            "4a5e1e4baab89f3a32518a88c31bc87f618f76673e2cc77ab2127b7afdeda33b"
        );
        assert_eq!(gen.header.time, 1296688602);
        assert_eq!(gen.header.bits, CompactTarget::from_consensus(0x1e0ffff0));
        assert_eq!(gen.header.nonce, 414098458);
        assert_eq!(
            gen.header.block_hash().to_string(),
            "000000000933ea01ad0ee984209779baaec3ced90fa3f408719526f8d77f4943"
        );
    }

    #[test]
    fn signet_genesis_full_block() {
        let gen = genesis_block(Network::Signet);
        assert_eq!(gen.header.version, block::Version::ONE);
        assert_eq!(gen.header.prev_blockhash, Hash::all_zeros());
        assert_eq!(
            gen.header.merkle_root.to_string(),
            "4a5e1e4baab89f3a32518a88c31bc87f618f76673e2cc77ab2127b7afdeda33b"
        );
        assert_eq!(gen.header.time, 1598918400);
        assert_eq!(gen.header.bits, CompactTarget::from_consensus(0x1e0377ae));
        assert_eq!(gen.header.nonce, 52613770);
        assert_eq!(
            gen.header.block_hash().to_string(),
            "00000008819873e925422c1ff0f99f7cc9bbb232af63a077a480a3633bee1ef6"
        );
    }

    // The *_chain_hash tests are sanity/regression tests, they verify that the const byte array
    // representing the genesis block is the same as that created by hashing the genesis block.
    fn chain_hash_and_genesis_block(network: Network) {
        use hashes::sha256;

        // The genesis block hash is a double-sha256 and it is displayed backwards.
        let genesis_hash = genesis_block(network).block_hash();
        // We abuse the sha256 hash here so we get a LowerHex impl that does not print the hex backwards.
        let hash = sha256::Hash::from_slice(genesis_hash.as_byte_array()).unwrap();
        let want = format!("{:02x}", hash);

        let chain_hash = ChainHash::using_genesis_block(network);
        let got = format!("{:02x}", chain_hash);

        // Compare strings because the spec specifically states how the chain hash must encode to hex.
        assert_eq!(got, want);

        #[allow(unreachable_patterns)] // This is specifically trying to catch later added variants.
        match network {
            Network::Bitcoin => {},
            Network::Testnet => {},
            Network::Signet => {},
            Network::Regtest => {},
            _ => panic!("Update ChainHash::using_genesis_block and chain_hash_genesis_block with new variants"),
        }
    }

    macro_rules! chain_hash_genesis_block {
        ($($test_name:ident, $network:expr);* $(;)*) => {
            $(
                #[test]
                fn $test_name() {
                    chain_hash_and_genesis_block($network);
                }
            )*
        }
    }

    chain_hash_genesis_block! {
        mainnet_chain_hash_genesis_block, Network::Bitcoin;
        testnet_chain_hash_genesis_block, Network::Testnet;
        signet_chain_hash_genesis_block, Network::Signet;
        regtest_chain_hash_genesis_block, Network::Regtest;
    }

    // Test vector taken from: https://github.com/lightning/bolts/blob/master/00-introduction.md
    #[test]
    fn mainnet_chain_hash_test_vector() {
        let got = ChainHash::using_genesis_block(Network::Bitcoin).to_string();
        let want = "6fe28c0ab6f1b372c1a6a246ae63f74f931e8365e15a089c68d6190000000000";
        assert_eq!(got, want);
    }
}
