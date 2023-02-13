{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}

module Main where

import Foreign.C.Types
import Control.Monad
import           Data.Vector.Storable (fromList)
import           Foreign.C.Types      (CInt)
import qualified SDL.Framerate
import qualified SDL.Primitive
import           SDL.Vect             (V2 (..), V4 (..))
import qualified SDL.Raw
import SDL
import SDL.Internal.Types
import GHC.Base (VecElem(Int16ElemRep))
import Foreign (ptrToWordPtr, Storable (peek, sizeOf, peekByteOff, pokeElemOff, peekElemOff, poke), castPtr, Ptr, ForeignPtr, newForeignPtr, with, mallocArray, malloc, free, Word32, mallocBytes, nullPtr, Word8)
import Control.Monad.IO.Class
import SDL.Video.Renderer
import SDL.Internal.Numbered
foreign import ccall "SDL.h SDL_free" freeFFI :: Ptr () -> IO ()
foreign import ccall "SDL.h SDL_memcmp" memcmpFFI :: Ptr () -> Ptr () -> CInt -> IO CInt
foreign import ccall "SDL.h SDL_RenderReadPixels" renderReadPixelsFFI :: SDL.Raw.Renderer -> Ptr SDL.Raw.Rect -> Word32 -> Ptr () -> CInt -> IO CInt

red :: SDL.Primitive.Color
red = V4 255 50 50 255

green :: SDL.Primitive.Color
green = V4 50 255 50 255

blue :: SDL.Primitive.Color
blue = V4 50 50 255 255

black :: SDL.Primitive.Color
black = V4 0 0 0 255

white :: SDL.Primitive.Color
white = V4 255 255 255 255

unmanagedSurface :: Ptr SDL.Raw.Surface -> SDL.Surface
unmanagedSurface s = SDL.Surface s Nothing

-- construct rectangle Rect for fillRect

r = SDL.Raw.Rect 10 10 10 10
mr = Just r
sxPtr :: Control.Monad.IO.Class.MonadIO m => m (Ptr SDL.Raw.Surface)
sxPtr = SDL.Raw.createRGBSurface 0 100 100 32 0 0 0 0

sx :: MonadIO m => m SDL.Surface
sx = fmap unmanagedSurface sxPtr
ptrRect = SDL.Raw.Rect 0 0 800 600

getPixels = fmap SDL.surfacePixels sx
myFillRect :: MonadIO m => SDL.Surface -> m()
myFillRect surface = SDL.surfaceFillRect surface Nothing (V4 255 0 255 255)
--b = (>>=) sx myFillRect

renderReadPixels :: MonadIO m => SDL.Raw.Renderer -> Ptr SDL.Raw.Rect -> Word32 -> Ptr () -> CInt -> m CInt
renderReadPixels v1 v2 v3 v4 v5 = liftIO $ renderReadPixelsFFI v1 v2 v3 v4 v5

data Circle = Circle Integer SDL.Primitive.Color

data Plane = Plane {size::Int, x::Int, y::Int, background::V4 Word8}
p = Plane 100 100 100 white

renderPlane :: Plane -> SDL.Raw.Renderer -> IO ()
renderPlane plane renderer = do
  SDL.Primitive.fillRectangle
   (SDL.Internal.Types.Renderer renderer)
   (V2  100 100)
   (V2 350 350)
   white


-- |radius -> color -> renderer -> texture 
myCircle :: CInt -> SDL.Primitive.Color -> SDL.Raw.Renderer -> IO SDL.Raw.Texture
myCircle radius color renderer = do
  tex <- SDL.Raw.createTexture
          renderer
          SDL.Raw.SDL_PIXELFORMAT_RGBA8888
          SDL.Raw.SDL_TEXTUREACCESS_TARGET
          (radius * 2 + 2)
          (radius * 2 + 2)
  SDL.Raw.setTextureBlendMode tex SDL.Raw.SDL_BLENDMODE_BLEND
  SDL.Raw.setRenderTarget renderer tex
  SDL.Primitive.circle (SDL.Internal.Types.Renderer renderer) (V2 radius radius) radius color
  SDL.Raw.setRenderTarget renderer nullPtr
  return tex

renderersEqual :: SDL.Raw.Renderer -> SDL.Raw.Renderer -> IO Bool
renderersEqual r0 r1 = do
  return True

main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo]
  (SDL.Internal.Types.Window w0) <- SDL.createWindow "" SDL.defaultWindow
  (SDL.Internal.Types.Window w1) <- SDL.createWindow "" SDL.defaultWindow
  --r <- SDL.createRenderer (SDL.Internal.Types.Window w) (-1) SDL.defaultRenderer
  rRaw0 <- SDL.Raw.createRenderer w0 (-1) (toNumber SDL.defaultRenderer)
  rRaw1 <- SDL.Raw.createRenderer w1 (-1) (toNumber SDL.defaultRenderer)
  --
  
  -- important: memcmp third parameter depends on the size of the surface
  s1 <- fmap unmanagedSurface (SDL.Raw.createRGBSurface 0 100 1000 32 0 0 0 0)
  s2 <- fmap unmanagedSurface (SDL.Raw.createRGBSurface 0 100 1000 32 0 0 0 0)
  -- fillRect is a effectful function, ...
  q <- myFillRect s1
  z <- myFillRect s2

  ss1 <- SDL.getWindowSurface (SDL.Internal.Types.Window w0)
  
  --SDL.present  r
  SDL.Raw.renderPresent rRaw0
  ss2 <- SDL.getWindowSurface (SDL.Internal.Types.Window w0)
  
  ss1Pixels <- SDL.surfacePixels ss1
  ss2Pixels <- SDL.surfacePixels ss2
  
  -- third param should be surface size dependant
  x <- memcmpFFI ss1Pixels ss2Pixels (800*600*4)   -- before was 400000

  width <- malloc :: IO (Ptr CInt)
  height <- malloc :: IO (Ptr CInt)

  xx <- SDL.Raw.getRendererOutputSize rRaw0 width height
  
    -- 29.12 -- doesn't work, pointer error

  buffer1 <- mallocBytes 1920000 :: IO (Ptr ())
  buffer2 <- mallocBytes 1920000 :: IO (Ptr ())
  
  --SDL.Primitive.circle (SDL.Internal.Types.Renderer rRaw0) (V2 200 200) 20 blue
  --tex <- SDL.Raw.createTexture rRaw0 SDL.Raw.SDL_PIXELFORMAT_RGB888 SDL.Raw.SDL_TEXTUREACCESS_TARGET 42 42
  --SDL.Raw.setTextureBlendMode tex SDL.Raw.SDL_BLENDMODE_BLEND
  --SDL.Raw.setRenderTarget rRaw0 tex
  --SDL.Primitive.circle (SDL.Internal.Types.Renderer rRaw0) (V2 20 20) 20 red
  --SDL.Raw.setRenderTarget rRaw0 nullPtr
  --SDL.Raw.setRenderDrawBlendMode rRaw0 SDL.Raw.SDL_BLENDMODE_BLEND
  circ1 <- myCircle 40 green rRaw0
  circ2 <- myCircle 80 blue rRaw0
  xxx <- with (SDL.Raw.Rect 20 20 82 82) $ \rectPtr -> do
    SDL.Raw.renderCopy rRaw0 circ1 nullPtr rectPtr

  xxx <- with (SDL.Raw.Rect 50 50 162 162) $ \rectPtr -> do
    SDL.Raw.renderCopy rRaw0 circ2 nullPtr rectPtr
    
  --SDL.Raw.renderPresent rRaw0

  zz1 <- with (SDL.Raw.Rect 0 0 800 600) $ \rectPtr -> do
    renderReadPixelsFFI rRaw0 rectPtr SDL.Raw.SDL_PIXELFORMAT_RGB888 buffer1 3200
  --SDL.Raw.renderClear rRaw
  SDL.Primitive.circle (SDL.Internal.Types.Renderer rRaw1) (V2 100 100) 20 blue
  SDL.Primitive.circle (SDL.Internal.Types.Renderer rRaw1) (V2 200 200) 20 red
  --SDL.Primitive.circle (SDL.Internal.Types.Renderer rRaw) (V2 100 100) 20 red
  renderPlane (Plane 100 100 100 white) rRaw1
  SDL.Raw.renderPresent rRaw1
  zz2 <- with (SDL.Raw.Rect 0 0 800 600) $ \rectPtr -> do
    renderReadPixelsFFI rRaw1 rectPtr SDL.Raw.SDL_PIXELFORMAT_RGB888 buffer2 3200

  x2 <- memcmpFFI buffer1 buffer2 (800*600*4)   -- before was 400000
  --free buffer
  gg <- peek width

  print x2
  --ws <- SDL.getWindowSurface w
  --ns <- SDL.surfaceFillRect ws Nothing (V4 100 3 66 100)
  --ms <- SDL.surfaceBlit s1 Nothing ws Nothing
  
  --SDL.updateWindowSurface w
  --
  --SDL.showWindow w
  
  SDL.delay 5000
  SDL.destroyWindow (SDL.Internal.Types.Window w0)
  SDL.destroyWindow (SDL.Internal.Types.Window w1)
  SDL.quit

loopFor :: CInt -> SDL.Window -> SDL.Renderer -> SDL.Surface -> SDL.Framerate.Manager  -> IO ()
loopFor limit window r s1 fpsm = loop'
  where
    loop' :: IO ()
    loop' = do
      -- How many frames have we drawn until now?
      frames <- fromIntegral `fmap` SDL.Framerate.count fpsm
      --print frames
      -- Clear the screen!
      SDL.rendererDrawColor r $= black
      SDL.clear r
      -- Run each of the functions from SDL.Primitives.
      -- For added chaos, move everything by framecount.
      --SDL.Primitive.pixel r (V2 (100 + frames) 100) green
      --SDL.Primitive.line r (V2 10 10) (V2 (25 + frames) (25 + frames)) red
      --SDL.Primitive.thickLine r (V2 10 15) (V2 (10 + frames) 120) 3 white
      --SDL.Primitive.smoothLine r (V2 100 frames) (V2 300 (20 - frames)) white
      --SDL.Primitive.horizontalLine r (V2 40 (350 + frames)) (2 * frames) blue
      --SDL.Primitive.verticalLine r (V2 40 (350 + frames)) (5 * frames) green
      --SDL.Primitive.rectangle r (V2 (75 + frames) (90 + frames)) (V2 (100 + frames) (100 + frames)) white
      --SDL.Primitive.roundRectangle r (V2 110 300) (V2 (170 + frames) (400 + frames)) 10 white
      --SDL.Primitive.fillRectangle r (V2 (175 + frames) (190 + frames)) (V2 (200 + frames) (200 + frames)) red
      --SDL.Primitive.fillRoundRectangle r (V2 120 310) (V2 (160 + frames) (390 + frames)) 5 blue
      --SDL.Primitive.arc r (V2 320 240) 100 0 (frames * 360 `div` limit) red
      --SDL.Primitive.circle r (V2 10 10) 20 red
      --SDL.Primitive.smoothCircle r (V2 320 240) 70 white
      --SDL.Primitive.fillCircle r (V2 320 240) 50 white
      --SDL.Primitive.ellipse r (V2 500 200) 70 (10 + frames) green
      --SDL.Primitive.smoothEllipse r (V2 500 200) 60 (5 + frames) white
      --SDL.Primitive.fillEllipse r (V2 500 200) 40 frames blue
      --SDL.Primitive.pie r (V2 640 500) 80 0 (frames * 360 `div` limit) red
      --SDL.Primitive.fillPie r (V2 640 400) 60 0 (frames * 360 `div` limit) blue
      --SDL.Primitive.triangle r (V2 700 10) (V2 750 10) (V2 750 60) red
      --SDL.Primitive.smoothTriangle r (V2 700 5) (V2 740 5) (V2 740 70) green
      --SDL.Primitive.fillTriangle r (V2 650 40) (V2 690 50) (V2 700 90) blue
      --SDL.Primitive.polygon r (fromList [100, 220, 430, 317, 50]) (fromList [30, 70, 200, 300, 500]) green
      --SDL.Primitive.smoothPolygon r (fromList $ map (+ 40) [100, 220, 430, 317, 50]) (fromList $ map (+ 40) [30, 70, 200, 300, 500]) blue
      --SDL.Primitive.fillPolygon r (fromList $ map (+ 300) [100, 220, 430, 317, 50]) (fromList $ map (+ 400) [30, 70, 200, 300, 500]) $ V4 200 20 20 128
      --SDL.Primitive.bezier r (fromList [70, 43, 23, 388, 239, 584, 444]) (fromList [546, 323, 110, 5, 483, 673, 332]) 5 $ V4 255 0 255 255

      SDL.present r
      SDL.Framerate.delay_ fpsm -- Delay to keep framerate constant.
      sur <- SDL.getWindowSurface window
      tex <- SDL.createTextureFromSurface r sur
      p <- SDL.surfacePixels sur
      d <- SDL.surfaceDimensions sur
      print d
      --re <- SDL.surfaceBlit s1 Nothing sur Nothing
      op <- myFillRect sur
      
      SDL.updateWindowSurface window
      when (frames < limit) loop'
      
