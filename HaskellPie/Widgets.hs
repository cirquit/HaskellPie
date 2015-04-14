module Widgets where

import Import
import Helper (formatDateStr, getLatestUpdate, spacesToMinus)

postWidget :: Enctype -> Widget -> Widget
postWidget enctype widget =  [whamlet|
    <form method=post enctype=#{enctype}>
        ^{widget}
                             |]

accountLinksW :: Widget
accountLinksW = do
    mnick <- lookupSession "_ID"
    case mnick of
        (Just nick) -> [whamlet|
            <a class="login" href=@{LogOutR}> Log out
            <a class="login" href=@{DeleteAccountR}> Delete account
            <a class="login" href=@{AccountR}> #{nick}
                       |]
        (_)         -> [whamlet|
            <a class="login" href=@{LogInR}> Log in
            <a class="login" href=@{SignUpR}> Sign up
                       |]

threadWidget :: Bool -- current user logged in && has moderator rights
             -> ThreadId
             -> Thread
             -> Widget
threadWidget isMod tid thread = do
   mnick <- lookupSession "_ID"
   nick <-  case mnick of
              (Just nick) -> return $ nick
              (_)         -> return $ ""
   let enum = [0..]::[Int]
   [whamlet|
        <div .thread_answer>
            $maybe pnick <- threadCreator thread
                 <a .simpleCreator href=@{UserR pnick}> #{pnick}
            $nothing
                <span .simpleCreator> Anonymous
            <span .simpleTime>#{formatDateStr $ show $ threadTime thread}
            $if isMod
                    <a href=@{EditThreadR tid}> Edit thread
                    <a href=@{DeleteThreadR tid}> Delete thread
            $else
                $maybe pnick <- threadCreator thread
                    $if (nick == pnick)
                        <a href=@{EditThreadR tid}> Edit thread
                        <a href=@{DeleteThreadR tid}> Delete thread
            <br>
            <span> #{threadContent thread}
    $maybe posts <- threadPosts thread
        $with enum_posts <- (zip enum posts)
            $forall (n, post) <- enum_posts
                <div .thread_answer>
                        $maybe pnick <- postCreator post
                            <a .simpleCreator href=@{UserR pnick}> #{pnick}
                        $nothing
                            <span .simpleCreator> Anonymous
                        <span .simpleTime> #{formatDateStr $ show $ postTime post}
                        $if isMod
                            <a href=@{EditPostR tid n}> Edit
                            <a href=@{DeletePostR tid n}> Delete
                        $else
                            $maybe pnick <- postCreator post
                                $if (nick == pnick)
                                    <a href=@{EditPostR tid n}> Edit
                                    <a href=@{DeletePostR tid n}> Delete
                        <br>
                        <span> #{postContent post}
                          |]


threadListWidget :: [Entity Thread] -> Widget
threadListWidget threads = [whamlet|
    <table .threads>
        $forall (Entity id thread) <- threads
            <tr .post>
                <td .threadname>
                    $if (length (threadTitle thread)) > 50
                        <a .threadname href=@{ThreadR $ spacesToMinus $ threadTitle thread}> #{take 50 $ threadTitle thread}...
                    $else
                        <a .threadname href=@{ThreadR $ spacesToMinus $ threadTitle thread}> #{threadTitle thread}
                <td .threadby>
                    by
                <td .username>
                    $maybe nick <- threadCreator thread
                         <a .username href=@{UserR nick}> #{nick}
                    $nothing
                        <span> Anonymous
                <td .update>
                    <span> Latest update: #{formatDateStr $ show $ getLatestUpdate thread}
|]
