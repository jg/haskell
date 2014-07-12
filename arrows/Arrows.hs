module Arrows where

import Control.Arrow
import System.IO

count w = Kleisli readFile >>> arr words >>> arr (filter (==w)) >>> arr length >>> Kleisli print
