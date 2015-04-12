module Widgets where

import Import
import Helper (formatDateStr, getLatestUpdate, spacesToMinus,
               cutBy20, isAdmin)

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

threadWidget :: ThreadId -> Thread -> Widget
threadWidget tid thread = do
   mnick <- lookupSession "_ID"
   nick <-  case mnick of
              (Just nick) -> return $ nick
              (_)         -> return $ ""
   let enum = [0..]::[Int]
   [whamlet|
        <div .thread_answer>
            $maybe (Person pnick _ _ _) <- threadCreator thread
                 <a .simpleCreator href=@{UserR pnick}> #{pnick}
            $nothing
                <span .simpleCreator> Anonymous
            <span .simpleTime>#{formatDateStr $ show $ threadTime thread}
            $if (isAdmin mnick)
                    <a href=@{EditThreadR tid}> Edit thread
                    <a href=@{DeleteThreadR tid}> Delete thread
            $else
                $maybe person <- threadCreator thread
                    $if nick == (personNick person)
                        <a href=@{EditThreadR tid}> Edit thread
                        <a href=@{DeleteThreadR tid}> Delete thread
            <br>
            <span> #{threadContent thread}
    $maybe posts <- threadPosts thread
        $with enum_posts <- (zip enum posts)
            $forall (n, post) <- enum_posts
                <div .thread_answer>
                        $maybe (Person pnick _ _ _)<- threadCreator thread
                            <a .simpleCreator href=@{UserR pnick}> #{pnick}
                        $nothing
                            <span .simpleCreator> Anonymous
                        <span .simpleTime> #{formatDateStr $ show $ postTime post}
                        $if (isAdmin mnick)
                            <a href=@{EditPostR tid n}> Edit
                            <a href=@{DeletePostR tid n}> Delete
                        $else
                            $maybe person <- postCreator post
                                $if nick == (personNick person)
                                    <a href=@{EditPostR tid n}> Edit
                                    <a href=@{DeletePostR tid n}> Delete
                        <br>
                        <span> #{postContent post}
                          |]

threadListWidget :: [Entity Thread] -> Widget
threadListWidget threads = [whamlet|
    <table #threadList>
        $forall (Entity id thread) <- threads
            <tr>
                $if (length (threadTitle thread)) > 20
                    <td .tooltips>
                        <a .threadLink href=@{ThreadR $ spacesToMinus $ threadTitle thread} style="margin:10px;"> #{cutBy20 $ threadTitle thread}
                        <span> #{threadTitle thread}
                $else
                    <td>
                        <a .threadLink href=@{ThreadR $ spacesToMinus $ threadTitle thread} style="margin:10px;"> #{cutBy20 $ threadTitle thread}
                <td>
                    $maybe (Person nick _ _ _)<- threadCreator thread
                         <a .simpleCreator href=@{UserR nick}> #{nick}
                    $nothing
                        <span .simpleCreator> Anonymous
                <td>
                    <span .simpleTime> Latest update: #{formatDateStr $ show $ getLatestUpdate thread}
|]