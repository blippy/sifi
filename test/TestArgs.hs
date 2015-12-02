module TestArgs where

import Args




args1 = ["-a", "--snap", "--init"]

t1 = compilerOpts args1

t2 = compilerOpts ["-a", "--start", "2015-11-01", "foo", "bar"]

t3 = compilerOpts ["-a"]

