{-# LANGUAGE TemplateHaskell #-}
module Widgets.Job
    ( jobsTable
    , jobsTableRow
    ) where

import Import

import GitHub.Data (toPathPart)
import Helper.Time

jobsTable :: [Entity Job] -> Widget
jobsTable jobs = $(widgetFile "widgets/jobs-table")

jobsTableRow :: Entity Job -> Widget
jobsTableRow job = $(widgetFile "widgets/jobs-table-row")
