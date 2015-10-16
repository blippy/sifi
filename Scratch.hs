{-# LANGUAGE TemplateHaskell #-}

import Utils

x = []
v1 = $(headQ 'x)
--v2 = headQ x
