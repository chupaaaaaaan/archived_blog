--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import Debug.Trace

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "js/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "posts/*" $ version "postContents" $ do
      route $ gsubRoute "posts/" (const "") `composeRoutes` setExtension "html"
      compile $ pandocCompiler >>= relativizeUrls

    match "posts/*" $ do
        route $ gsubRoute "posts/" (const "") `composeRoutes` setExtension "html"
        compile $ do
            recentPosts <- recentFirst =<< loadAll ("posts/*" .&&. hasVersion "postContents")
            let postsCtx =
                    listField "recent_posts" postCtx (return $ take 5 recentPosts) `mappend`
                    postCtx `mappend` siteCtx
          
            pandocCompiler
              >>= loadAndApplyTemplate "layouts/post.html" postsCtx
              >>= relativizeUrls

    match (fromList ["archive.html", "about.html", "index.html", "links.html"]) $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll ("posts/*" .&&. hasVersion "postContents")
            let postsCtx =
                    listField "recent_posts" postCtx (return $ take 5 posts) `mappend`
                    listField "posts" postCtx (return posts) `mappend`
                    siteCtx

            getResourceBody
                >>= applyAsTemplate postsCtx
                >>= loadAndApplyTemplate "layouts/default.html" postsCtx
                >>= relativizeUrls

    create ["atom.xml"] $ do
        route idRoute
        compile $ do
          let feedCtx = constField "description" "feed description" `mappend` postCtx `mappend` siteCtx
          posts <- recentFirst =<< loadAll ("posts/*" .&&. hasVersion "postContents")
          renderAtom feedConf feedCtx (take 5 posts)


    match "layouts/*" $ compile templateBodyCompiler
    match "includes/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
siteCtx :: Context String
siteCtx =
  boolField "comments" (const False) `mappend`
  constField "site_name" "コーヒーと線香と万年筆" `mappend`
  constField "site_description" "勉強したことおきば" `mappend`
  constField "github" "chupaaaaaaan" `mappend`
  constField "qiita" "chupaaaaaaan" `mappend`
  defaultContext

postCtx :: Context String
postCtx =
  dateField "date" "%Y-%m-%d (%a)" `mappend`
  dateField "year" "%Y" `mappend`
  defaultContext
  
feedConf :: FeedConfiguration
feedConf = FeedConfiguration
    { feedTitle       = "コーヒーと線香と万年筆: 最近の投稿"
    , feedDescription = "This feed provides fresh recipes for fresh food!"
    , feedAuthorName  = "Takayuki Uchida"
    , feedAuthorEmail = ""
    , feedRoot        = "https://chupaaaaaaan.github.io"
    }
