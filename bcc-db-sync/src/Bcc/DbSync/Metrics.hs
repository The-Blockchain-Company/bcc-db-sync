{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Bcc.DbSync.Metrics
  ( Metrics (..)
  , makeMetrics
  , withMetricSetters
  , withMetricsServer
  ) where

import           Bcc.Prelude

import           Bcc.Slotting.Slot (SlotNo (..))

import           Bcc.Sync.Types (MetricSetters (..))

import           Shardagnostic.Network.Block (BlockNo (..))

import           System.Metrics.Prometheus.Concurrent.RegistryT (RegistryT (..), registerGauge,
                   runRegistryT, unRegistryT)
import           System.Metrics.Prometheus.Http.Scrape (serveMetricsT)
import           System.Metrics.Prometheus.Metric.Gauge (Gauge)
import qualified System.Metrics.Prometheus.Metric.Gauge as Gauge

data Metrics = Metrics
  { mNodeBlockHeight :: !Gauge
  -- ^ The block tip number of the remote node.
  , mDbQueueLength :: !Gauge
  -- ^ The number of @DbAction@ remaining for the database.
  , mDbBlockHeight :: !Gauge
  -- ^ The block tip number in the database.
  , mDbSlotHeight :: !Gauge
  -- ^ The slot tip number in the database.
  }

-- This enables us to be much more flexibile with what we actually measure.
withMetricSetters :: Int -> (MetricSetters -> IO a) -> IO a
withMetricSetters prometheusPort action =
  withMetricsServer prometheusPort $ \metrics -> do
    action $
      MetricSetters
        { metricsSetNodeBlockHeight = \ (BlockNo nodeHeight) ->
            Gauge.set (fromIntegral nodeHeight) $ mNodeBlockHeight metrics
        , metricsSetDbQueueLength = \ queuePostWrite ->
            Gauge.set (fromIntegral queuePostWrite) $ mDbQueueLength metrics
        , metricsSetDbBlockHeight = \ (BlockNo blockNo) ->
            Gauge.set (fromIntegral blockNo) $ mDbBlockHeight metrics
        , metricsSetDbSlotHeight = \ (SlotNo slotNo) ->
            Gauge.set (fromIntegral slotNo) $ mDbSlotHeight metrics
        }

withMetricsServer :: Int -> (Metrics -> IO a) -> IO a
withMetricsServer port action = do
  -- Using both `RegistryT` and `bracket` here is overkill. Unfortunately the
  -- Prometheus API requires the use of a `Registry` and this seems to be the
  -- least sucky way of doing it.
  (metrics, registry) <- runRegistryT $ (,) <$> makeMetrics <*> RegistryT ask
  bracket
     (async $ runReaderT (unRegistryT $ serveMetricsT port []) registry)
     cancel
     (const $ action metrics)

makeMetrics :: RegistryT IO Metrics
makeMetrics =
  Metrics
    <$> registerGauge "bcc_db_sync_node_block_height" mempty
    <*> registerGauge "bcc_db_sync_db_queue_length" mempty
    <*> registerGauge "bcc_db_sync_db_block_height" mempty
    <*> registerGauge "bcc_db_sync_db_slot_height" mempty
