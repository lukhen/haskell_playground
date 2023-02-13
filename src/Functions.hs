module Functions where

import Foreign (Ptr, mallocBytes, with, malloc, Storable (peek))
import qualified SDL.Raw
import Foreign.C.Types
foreign import ccall "SDL.h SDL_memcmp" memcmpFFI :: Ptr () -> Ptr () -> CInt -> IO CInt

-- | Compare renderers' pixels.
--   This function is very badly written.
--   I use it for learning purposes and testing while learning. It should be improved.
--   Most of all the memory isn't freed after operations on pointers.
--   For now my haskell knowledge is insufficient to do it properly.
--   I found this https://ro-che.info/articles/2017-08-06-manage-allocated-memory-haskell
--   but the article introduces a lot of new concepts so for now I leave it as it is.
--   I'll come back to improve it in some time.
--   
renderersEqual :: SDL.Raw.Renderer -> SDL.Raw.Renderer -> IO Bool
renderersEqual r0 r1 = do
  r0WidthPtr <- malloc :: IO (Ptr CInt)
  r0HeightPtr <- malloc :: IO (Ptr CInt)
  _ <- SDL.Raw.getRendererOutputSize r0 r0WidthPtr r0HeightPtr
  r1WidthPtr <- malloc :: IO (Ptr CInt)
  r1HeightPtr <- malloc :: IO (Ptr CInt)
  _ <- SDL.Raw.getRendererOutputSize r1 r1WidthPtr r1HeightPtr
  r0Width <- peek r0WidthPtr
  r0Height <- peek r0HeightPtr
  r1Width <- peek r1WidthPtr
  r1Height <- peek r1HeightPtr
  if r0Width /= r1Width || r0Height /= r1Height
    then return False
    else do
    bpp <- do return 4  -- bytes per pixel
    buffer1 <- mallocBytes (fromIntegral (r0Width*r0Height*bpp)) :: IO (Ptr ())
    buffer2 <- mallocBytes (fromIntegral (r0Width*r0Height*bpp)) :: IO (Ptr ())
    _ <- with (SDL.Raw.Rect 0 0 (fromIntegral r0Width) (fromIntegral r0Height)) $ \rectPtr -> do
      SDL.Raw.renderReadPixels r0 rectPtr SDL.Raw.SDL_PIXELFORMAT_RGB888 buffer1 (fromIntegral (r0Width*bpp))
    _ <- with (SDL.Raw.Rect 0 0 (fromIntegral r0Width) (fromIntegral r0Height)) $ \rectPtr -> do
      SDL.Raw.renderReadPixels r1 rectPtr SDL.Raw.SDL_PIXELFORMAT_RGB888 buffer2 (fromIntegral (r0Width*bpp))
    fmap (== 0) $ memcmpFFI buffer1 buffer2 (fromIntegral (r0Width*r0Height*bpp))

  
