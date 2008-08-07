
-- XXX this doesn't work because I don't really know what I'm doing.
-- I want to pipe my own stdout to less with some args.
-- I think the right way to go is use System.Posix.Process, rather than
-- the higher-level System.Process I'm using here, to be able to dup2 in
-- the child process.
redirectThroughLess :: IO ()
redirectThroughLess = do
  (rd, wr) <- createPipe
  --dupTo 0 rd
  dupTo 1 wr
  pipeIn <- fdToHandle rd
  lesspid <- runProcess "less" [] --["-F", "-R", "-S", "-X"]
                        Nothing Nothing
                        (Just pipeIn) Nothing Nothing
  return ()

