module Graphics.Uniforms (BinaryThresholdUni, WindowLevelUni, NormalizeUni, Uniforms(..)) where

import           Data.Word      (Word32)
import           Graphics.GPipe (B, Buffer, Uniform)

type BinaryThresholdUni os = (Buffer os (Uniform (B Word32)), Int)

type WindowLevelUni os = (Buffer os (Uniform (B Word32, B Word32)), Int)

type NormalizeUni os = (Buffer os (Uniform (B Word32, B Word32, B Word32, B Word32)), Int)

data Uniforms os = Uniforms {
  binaryUni      :: BinaryThresholdUni os,
  windowLevelUni :: WindowLevelUni os,
  normUni        :: NormalizeUni os
  }
