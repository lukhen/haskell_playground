{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE OverloadedStrings #-}

module TexturesEqualSpec where

import Test.Hspec
import qualified SDL.Raw
import SDL
import SDL.Internal.Types
import SDL.Internal.Numbered
import qualified SDL.Primitive
import Functions


pow2 :: Integer -> Integer
pow2 = (^2)

spec :: Spec
spec = do
  describe "testuresEqual" $ do
    it "just plain renderer" $ do
      (SDL.Internal.Types.Window w0) <- SDL.createWindow "" SDL.defaultWindow
      r0 <- SDL.Raw.createRenderer w0 (-1) (toNumber SDL.defaultRenderer)
      (SDL.Internal.Types.Window w1) <- SDL.createWindow "" SDL.defaultWindow
      r1 <- SDL.Raw.createRenderer w1 (-1) (toNumber SDL.defaultRenderer)
      res <- renderersEqual r0 r1
      res `shouldBe` True

    it "one pixel somewhere in the middle of renderer 0" $ do
      (SDL.Internal.Types.Window w0) <- SDL.createWindow "" SDL.defaultWindow
      r0 <- SDL.Raw.createRenderer w0 (-1) (toNumber SDL.defaultRenderer)
      (SDL.Internal.Types.Window w1) <- SDL.createWindow "" SDL.defaultWindow
      r1 <- SDL.Raw.createRenderer w1 (-1) (toNumber SDL.defaultRenderer)
      SDL.Primitive.pixel (SDL.Internal.Types.Renderer r0) (V2 100 100) (V4 50 255 50 255)
      res <- renderersEqual r0 r1
      res `shouldBe` False

    it "one pixel at (0, 0) of renderer 0" $ do
      (SDL.Internal.Types.Window w0) <- SDL.createWindow "" SDL.defaultWindow
      r0 <- SDL.Raw.createRenderer w0 (-1) (toNumber SDL.defaultRenderer)
      (SDL.Internal.Types.Window w1) <- SDL.createWindow "" SDL.defaultWindow
      r1 <- SDL.Raw.createRenderer w1 (-1) (toNumber SDL.defaultRenderer)
      SDL.Primitive.pixel (SDL.Internal.Types.Renderer r0) (V2 0 0) (V4 50 255 50 255)
      res <- renderersEqual r0 r1
      res `shouldBe` False

    it "one pixel at (0, 0) on both renderers " $ do
      (SDL.Internal.Types.Window w0) <- SDL.createWindow "" SDL.defaultWindow
      r0 <- SDL.Raw.createRenderer w0 (-1) (toNumber SDL.defaultRenderer)
      (SDL.Internal.Types.Window w1) <- SDL.createWindow "" SDL.defaultWindow
      r1 <- SDL.Raw.createRenderer w1 (-1) (toNumber SDL.defaultRenderer)
      SDL.Primitive.pixel (SDL.Internal.Types.Renderer r0) (V2 0 0) (V4 50 255 50 255)
      SDL.Primitive.pixel (SDL.Internal.Types.Renderer r1) (V2 0 0) (V4 50 255 50 255)
      res <- renderersEqual r0 r1
      res `shouldBe` True

    it "different sizes" $ do
      wc <- do return SDL.WindowConfig
                {windowBorder          = True
                , windowHighDPI         = False
                , windowInputGrabbed    = False
                , windowMode            = Windowed
                , windowGraphicsContext = NoGraphicsContext
                , windowPosition        = Wherever
                , windowResizable       = False
                , windowInitialSize     = V2 100 100  -- changed window dimensions
                , windowVisible      = True
                }
      (SDL.Internal.Types.Window w0) <- SDL.createWindow "" SDL.defaultWindow
      r0 <- SDL.Raw.createRenderer w0 (-1) (toNumber SDL.defaultRenderer)
      (SDL.Internal.Types.Window w1) <- SDL.createWindow "" wc
      r1 <- SDL.Raw.createRenderer w1 (-1) (toNumber SDL.defaultRenderer)
      res <- renderersEqual r0 r1
      res `shouldBe` False

