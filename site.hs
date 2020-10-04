--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Semigroup
import           Hakyll
import qualified Skylighting

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    create ["css/highlight.css"] $ do
        route   idRoute
        compile $ makeItem (compressCss $ Skylighting.styleToCss Skylighting.pygments)

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "js/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "posts/*" $ version "postContents" $ do
      route $ gsubRoute "posts/" (const "") `composeRoutes` setExtension "html"
      compile $ pandocCompiler
        >>= saveSnapshot "content"
        >>= relativizeUrls

    match "posts/*" $ do
        route $ gsubRoute "posts/" (const "") `composeRoutes` setExtension "html"
        compile $ do
            recentPosts <- recentFirst =<< loadAll ("posts/*" .&&. hasVersion "postContents")
            let postsCtx =
                    listField "recent_posts" postCtx (return $ take 5 recentPosts) <>
                    postCtx <> siteCtx

            pandocCompiler
              >>= loadAndApplyTemplate "layouts/post.html" postsCtx
              >>= relativizeUrls

    match (fromList ["archive.html", "about.html", "index.html", "links.html"]) $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll ("posts/*" .&&. hasVersion "postContents")
            let postsCtx =
                    listField "recent_posts" (teaserField "teaser" "content" <> postCtx) (return $ take 5 posts) <>
                    listField "posts" postCtx (return posts) <>
                    siteCtx

            getResourceBody
                >>= applyAsTemplate postsCtx
                >>= loadAndApplyTemplate "layouts/default.html" postsCtx
                >>= relativizeUrls

    create ["atom.xml"] $ do
        route idRoute
        compile $ do
          let feedCtx = constField "description" "feed description" <> postCtx <> siteCtx
          posts <- recentFirst =<< loadAll ("posts/*" .&&. hasVersion "postContents")
          renderAtom feedConf feedCtx (take 5 posts)


    match "layouts/*" $ compile templateBodyCompiler
    match "includes/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
siteCtx :: Context String
siteCtx =
  boolField "comments" (const False) <>
  constField "site_name" "コーヒーと線香と万年筆" <>
  constField "site_description" "勉強したことおきば" <>
  constField "github" "chupaaaaaaan" <>
  constField "qiita" "chupaaaaaaan" <>
  defaultContext

postCtx :: Context String
postCtx =
  dateField "date" "%Y-%m-%d (%a)" <>
  dateField "year" "%Y" <>
  defaultContext

feedConf :: FeedConfiguration
feedConf = FeedConfiguration
    { feedTitle       = "コーヒーと線香と万年筆: 最近の投稿"
    , feedDescription = "This feed provides fresh recipes for fresh food!"
    , feedAuthorName  = "Takayuki Uchida"
    , feedAuthorEmail = ""
    , feedRoot        = "https://chupaaaaaaan.github.io"
    }
