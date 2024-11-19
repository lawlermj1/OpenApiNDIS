
# OpenApiNDIS  

This is a parser of the NDIS OpenAPI or swagger JSON spec.  

This was developed by Matthew Lawler. 

## Specification 
This module parses the NDIS OpenAPI specification file. 

   Ref: https://www.ndis.gov.au/providers/working-provider/connecting-ndia-systems 
   
## Usage 

   Just compile the hs files. 
   ndis.swagger.json is the input files. 
   It produces multiple csv files for each OpenAPI type. 
 

## Design Patterns used

  There are two tricky partial type patterns. Inspecting them requires some deep type dives. 

  1. Definitions 

  type Definitions = InsOrdHashMap Text
  _componentsSchemas :: Definitions Schema 

  This is a partial application of InsOrdHashMap. 
  The Text represents the key value for whatever the second type is. 
  So _componentsSchemas can be turned into a (k,v) = (Text,Schema). 

  2. Referenced 
   data Referenced a = Ref Reference | Inline a 
   _pathItemParameters :: [Referenced Param]

  This is really a kind of Either. 
  It allows the specification of a reference or an instance of a type. 
  In schemas, it can be thought of as a reference to another table, or a field definition. 
  In the spec, these are called $ref 

  3. Aeson functions used 
  encode and decode are Aeson commands
  needs Aeson and swagger 
  encode (mempty :: Swagger)
  "{\"swagger\":\"2.0\",\"info\":{\"title\":\"\",\"version\":\"\"}}"
  decode $ encode (mempty :: Swagger)
  Just ()
  encode :: ToJSON a => a -> ByteString 
  encodeFile :: ToJSON a => FilePath -> a -> IO ()
  decodeStrict :: FromJSON a => ByteString -> Maybe a 
  decodeFileStrict :: FromJSON a => FilePath -> IO (Maybe a)

  4. Schemas and Objects 
  Treat Object is an entity type. It can also be an OO object with a method! E.g. AddGroup. 
  Schema properties are Entity attributes. 

## Background 

  OpenAPI was originally called swagger. 
  https://swagger.io/specification/

  Nickolay Kudasov has developed the Swagger library, which is now replaced by the OpenAPi library. 

  Other swagger and API links: 

  https://editor.swagger.io/

  https://apis.guru/

## Done
 
0.	Text 
Data.HashMap.Strict.InsOrd as IOHM  insert-ordered-containers
Data.Text text

1.	Use swagger to discover hs types 
1a.	swagger -> paramschema cvs
1b. swagger -> schema csv (per path)
2.	swagger -> csv 

## To do 

Parse out key words from the name e.g. AddGroup => Add Group 

3.	swagger to DDL 
4.	encode swagger -> Hs Type 
5.	diff swagger with original until all parsed use compaREST 
https://github.com/typeable/compaREST 
6.  API code genration 

## Code generation

Parser here
https://github.com/owainlewis/openapi/tree/master

Code generator here 
https://github.com/Haskell-OpenAPI-Code-Generator/Haskell-OpenAPI-Client-Code-Generator

Next level codegen using servant 
https://github.com/swagger-api/swagger-codegen

https://github.com/OpenAPITools/openapi-generator/blob/master/docs/generators/haskell.md

https://github.com/OpenAPITools/openapi-generator/blob/master/docs/generators/haskell.md

## Issues - closed 
   0. JSON requires serious, byzantine type foo 
   1. Couldn't match type ‘http-media-0.8.1.1:Network.HTTP.Media.MediaType.Internal.MediaType’ with ‘MediaType’
   rb2Medias :: Referenced RequestBody -> [(MediaType, MediaTypeObject)] 
   rb2Medias rrb = map (\(k,v) -> (k, mto2ref v)) (IOHM.toList (_requestBodyContent rrb)) 
    Solved by not defining the type - relaxing the function definition 

   2. Number parsing for Enum values 
    Resolved through loss typing, and string parsing 

   3. Scientific module mismatch - coerce does not work 
    package match error error 
    This seems to be caused by using ! for strictness on the Value type. 
    Couldn't match expected type `Scientific'  with actual type `scientific-0.3.7.0:Data.Scientific.Scientific' 
    ignored as there is a show instance anyway 

## Issues - open 
   10. Incomplete parsing due to unspecified types by ndis at the top level. 
     e.g. Callbacks, Examples, ExternalDocs, Headers, Links, Responses, Servers, Tags 
    some of these occur under PathItems. 
    all Responses are always 200 - not other http codes defined. 
    a smale number of paths have multiple operations. 
    Most of the are just one, such as get, put, etc. 

  11. Link - Not in scope: type constructor or class ‘ExpressionOrValue’  
    Not in scope: type constructor or class ‘ExpressionOrValue’ 
    defaultExpressionOrValue :: ExpressionOrValue 
    defaultExpressionOrValue = defaultValue 

