module Widgets where

import Import
import Helper (formatDateStr, spacesToMinus, calculateDiffTime)

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
                 <a .username href=@{UserR pnick}> #{pnick}
            $nothing
                <span .username> Anonymous
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
                            <a .username href=@{UserR pnick}> #{pnick}
                        $nothing
                            <span .username> Anonymous
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
                        #{postContent post}
                          |]


threadListWidget :: [Entity Thread] -> Int -> Widget
threadListWidget threads maxlength = do
    cur <- liftIO $ getCurrentTime
    [whamlet|
    <table .threads>
        $forall (Entity id thread) <- threads
            <tr .post>
                <td .threadname>
                    $if (length (threadTitle thread)) > maxlength
                        <a .threadname href=@{ThreadR $ spacesToMinus $ threadTitle thread}> #{take maxlength $ threadTitle thread}...
                    $else
                        <a .threadname href=@{ThreadR $ spacesToMinus $ threadTitle thread}> #{threadTitle thread}
                <td .threadby>
                    <a .threadby href=@{ThreadR $ spacesToMinus $ threadTitle thread}> by
                <td .username>
                    $maybe nick <- threadCreator thread
                         <a .username href=@{UserR nick}> #{nick}
                    $nothing
                        <a ..username href=@{ThreadR $ spacesToMinus $ threadTitle thread}>Anonymous
                <td .threadby>
                    $maybe posts <- threadPosts thread
                        <a .threadby href=@{ThreadR $ spacesToMinus $ threadTitle thread}> P: #{length posts}
                    $nothing
                        <a .threadby href=@{ThreadR $ spacesToMinus $ threadTitle thread}> P: 0
                <td .update>
                    <a .update href=@{ThreadR $ spacesToMinus $ threadTitle thread}>Updated #{calculateDiffTime cur thread} ago
            |]
