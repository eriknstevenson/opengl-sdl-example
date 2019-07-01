{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import qualified Data.ByteString as BS
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Graphics.Rendering.OpenGL 
import SDL hiding (clear)


triangle :: [GLfloat]
triangle = 
  [-1.0, -1.0, 0.0
  , 1.0, -1.0, 0.0
  , 0.0,  1.0, 0.0
  ]


main :: IO ()
main = do
  initializeAll

  let windowConfig = 
        defaultWindow 
          { windowOpenGL = Just defaultOpenGL
          } 

  window <- createWindow "app" windowConfig
  ctx <- glCreateContext window
  
  vao <- genObjectName
  bindVertexArrayObject $= Just vao

  vertexBuffer <- genObjectName
  bindBuffer ArrayBuffer $= Just vertexBuffer

  withArrayLen triangle $ \count ptr ->
    bufferData ArrayBuffer $= 
      ( fromIntegral $ count * sizeOf (undefined::GLfloat)
      , ptr
      , StaticDraw)

  program <- makeProgram
  currentProgram $= Just program

  appLoop window vertexBuffer

  glDeleteContext ctx
  putStrLn "Thanks for playing."


appLoop :: Window -> BufferObject -> IO ()
appLoop window vb = do
  clearColor $= Color4 1.0 0.1 0.4 1.0
  clear [ColorBuffer]

  vertexAttribArray (AttribLocation 0) $= Enabled
  bindBuffer ArrayBuffer $= Just vb
  vertexAttribPointer (AttribLocation 0) $= 
    ( ToFloat
    , VertexArrayDescriptor 3 Float 0 nullPtr)
  drawArrays Triangles 0 3
  vertexAttribArray (AttribLocation 0) $= Disabled
  glSwapWindow window
  
  events <- pollEvents
  unless (any (keyPressed KeycodeQ) events) $ appLoop window vb


keyPressed :: Keycode -> Event -> Bool
keyPressed key event =
  case eventPayload event of
    KeyboardEvent keyboardEvent ->
      keyboardEventKeyMotion keyboardEvent == Pressed &&
      keysymKeycode (keyboardEventKeysym keyboardEvent) == key
    _ -> False


loadShader :: ShaderType -> FilePath -> IO Shader
loadShader t path = do
  shader <- createShader t
  source <- BS.readFile path
  shaderSourceBS shader $= source
  compileShader shader
  status <- get (compileStatus shader)
  unless status $ 
      get (shaderInfoLog shader) 
        >>= putStrLn . ("Shader failed to compile: " ++)
  return shader

makeProgram :: IO Program
makeProgram = do

  let shaders = 
        [ (VertexShader, "shader.vert")
        , (FragmentShader, "shader.frag")
        ]

  program <- createProgram

  forM_ shaders $ \(t, path) ->
    loadShader t path >>= attachShader program

  linkProgram program
  return program

