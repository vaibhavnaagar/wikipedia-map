# Wikipedia-Map

* An API server and parser to get the wiki links and page titles of wikipedia articles using [MediaWiki API](https://www.mediawiki.org/wiki/API:Main_page). API server runs over Haskell stack and clients or web applications request particular actions by suitably choosing query paramters and API endpoint over HTTP.

# Code Structure
* `src/WikiApiService.hs`: Send GET requests to MediaWiki API with query paramters set by the API server and get data in JSON format.
* `src/WikiParser.hs`: Parses JSON data received from `WikiApiService.hs` and also parses raw HTML text to get wiki links. The file contains several utility functions to extract the first N wiki links or extract all wiki links from the first N paragraphs from the HTML text.     
* `src/ServeApi.hs`: Consists of utility functions to create API server and methods for different endpoints supported by the API. API server typically runs on `localhost:7777` and includes endpoints:
      1. `/links?page=string`: Requires wiki page name as a query parameter and replies with the list of links in JSON format   
      2. `/pagename?page=string`: Replies accurate wiki page name from the page name provided in the query parameter. This works only when the given page name is an alias in the MediaWiki API.
      3. `/random`: Replies random accurate wiki page name.
* `test/Spec.hs`: A CLI based method to test WikiApiService and WikiParser in which user provides two wiki page names (starting point and ending point) and application successively traverses through the first non-parenthesized link in the main text of wikipedia page which begins from starting page until it reaches the ending page or the loop is detected.

# How To Use
* Install Haskell stack and all other dependencies using `package.yaml`
* Run API server using
``` stack exec wikipedia-map-exe
```
* To run single file use
``` stack ghci filename.hs
```
* Use frontend code `test/wikipedia-map-frontend` for visualizing the connections between wikipedia pages in the browser.
    * Thanks to **[Luke Deen Taylor](https://github.com/controversial/wikipedia-map)** for the frontend.
    * Frontend code is modified to add support for distinguishing the first wiki link from the rest by coloring those nodes and edges differently.
    * Change the `api_endpoint` variable in `test/wikipedia-map-frontend/js/api.js` and point it to the API server. Eg: *http://localhost:7777/*
    * Run `index.html` file in the browser to see the web application.

# Requirements
* [Haskell stack](https://docs.haskellstack.org/en/stable/README/)
* Dependencies can be found in `package.yaml`
