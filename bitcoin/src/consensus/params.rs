// SPDX-License-Identifier: CC0-1.0

//! Bitcoin consensus parameters.
//!
//! This module provides a predefined set of parameters for different Bitcoin
//! chains (such as mainnet, testnet).
//!

use crate::network::Network;
use crate::pow::Target;

/// Parameters that influence chain consensus.
#[non_exhaustive]
#[derive(Debug, Clone)]
pub struct Params {
    /// Network for which parameters are valid.
    pub network: Network,
    /// Time when BIP16 becomes active.
    pub bip16_time: u32,
    /// Block height at which BIP34 becomes active.
    pub bip34_height: u32,
    /// Block height at which BIP65 becomes active.
    pub bip65_height: u32,
    /// Block height at which BIP66 becomes active.
    pub bip66_height: u32,
    /// Minimum blocks including miner confirmation of the total of 2016 blocks in a retargeting period,
    /// (nPowTargetTimespan / nPowTargetSpacing) which is also used for BIP9 deployments.
    /// Examples: 1916 for 95%, 1512 for testchains.
    pub rule_change_activation_threshold: u32,
    /// Number of blocks with the same set of rules.
    pub miner_confirmation_window: u32,
    /// Proof of work limit value. It contains the lowest possible difficulty.
    ///
    /// Note that this value differs from Bitcoin Core's powLimit field in that this value is
    /// attainable, but Bitcoin Core's is not. Specifically, because targets in Bitcoin are always
    /// rounded to the nearest float expressible in "compact form", not all targets are attainable.
    /// Still, this should not affect consensus as the only place where the non-compact form of
    /// this is used in Bitcoin Core's consensus algorithm is in comparison and there are no
    /// compact-expressible values between Bitcoin Core's and the limit expressed here.
    pub pow_limit: Target,
    /// Expected amount of time to mine one block.
    pub pow_target_spacing: u64,
    /// Difficulty recalculation interval.
    pub pow_target_timespan: u64,
    /// Determines whether minimal difficulty may be used for blocks or not.
    pub allow_min_difficulty_blocks: bool,
    /// Determines whether retargeting is disabled for this network or not.
    pub no_pow_retargeting: bool,
}

impl Params {
    /// Creates parameters set for the given network.
    pub fn new(network: Network) -> Self {
        match network {
            Network::Bitcoin => Params {
                network: Network::Bitcoin,
                bip16_time: 1333238400,  // Apr 1 2012
                // From Junocash chainparams.cpp consensus section
                bip34_height: 0x210c,    // 8460 from consensus
                bip65_height: 0x210c,    // 8460 from consensus 
                bip66_height: 0x210c,    // 8460 from consensus
                rule_change_activation_threshold: 9576, // 95% of 10,080
                miner_confirmation_window: 10080,      // 60 * 24 * 7 = 10,080 blocks, or one week
                pow_limit: Target::MAX_ATTAINABLE_MAINNET,
                pow_target_spacing: 75,   // POST_BLOSSOM: 75 seconds
                pow_target_timespan: 24 * 60 * 60, // 1 day = 86400 seconds
                allow_min_difficulty_blocks: false,
                no_pow_retargeting: false,
            },
            Network::Testnet => Params {
                network: Network::Testnet,
                bip16_time: 1333238400,
                bip34_height: 99999999,  // From Junocash consensus
                bip65_height: 99999999,  // From Junocash consensus
                bip66_height: 99999999,  // From Junocash consensus
                rule_change_activation_threshold: 9576, // 95% of 10,080
                miner_confirmation_window: 10080,
                pow_limit: Target::MAX_ATTAINABLE_TESTNET,
                pow_target_spacing: 75,   // POST_BLOSSOM: 75 seconds
                pow_target_timespan: 14400, // 4 hours from Junocash consensus
                allow_min_difficulty_blocks: true,
                no_pow_retargeting: false,
            },
            Network::Signet => Params {
                network: Network::Signet,
                bip16_time: 1333238400,
                bip34_height: 99999999,
                bip65_height: 99999999,
                bip66_height: 99999999,
                rule_change_activation_threshold: 9576,
                miner_confirmation_window: 10080,
                pow_limit: Target::MAX_ATTAINABLE_SIGNET,
                pow_target_spacing: 75,   // POST_BLOSSOM: 75 seconds
                pow_target_timespan: 14400, // 4 hours
                allow_min_difficulty_blocks: true,
                no_pow_retargeting: false,
            },
            Network::Regtest => Params {
                network: Network::Regtest,
                bip16_time: 1333238400,
                bip34_height: 99999999,
                bip65_height: 99999999,
                bip66_height: 99999999,
                rule_change_activation_threshold: 9576,
                miner_confirmation_window: 10080,
                pow_limit: Target::MAX_ATTAINABLE_REGTEST,
                pow_target_spacing: 75,   // POST_BLOSSOM: 75 seconds
                pow_target_timespan: 14400, // 4 hours
                allow_min_difficulty_blocks: true,
                no_pow_retargeting: false,
            },
        }
    }

    /// Calculates the number of blocks between difficulty adjustments
    pub fn difficulty_adjustment_interval(&self) -> u64 {
        self.pow_target_timespan / self.pow_target_spacing
    }
}
