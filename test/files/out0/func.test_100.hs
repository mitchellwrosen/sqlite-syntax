Statement'Select
    ( SelectStatement
        { commonTableExpressions = Nothing
        , select = CompoundSelect
            ( SelectCore'Select
                ( Select
                    { distinct = False
                    , columns = ResultColumn'Expression
                        ( Aliased
                            { value = Expression'FunctionCall
                                ( FunctionCallExpression
                                    { call = FunctionCall
                                        { name = Namespaced
                                            { namespace = Nothing
                                            , value = "testfunc"
                                            }
                                        , arguments = FunctionArguments'Arguments
                                            [ Expression'LiteralValue
                                                ( String "string" )
                                            , Expression'LiteralValue
                                                ( String "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" )
                                            , Expression'LiteralValue
                                                ( String "int" )
                                            , Expression'LiteralValue
                                                ( Number "1234" )
                                            , Expression'LiteralValue
                                                ( String "string" )
                                            , Expression'LiteralValue
                                                ( String "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" )
                                            , Expression'LiteralValue
                                                ( String "string" )
                                            , Expression'LiteralValue Null
                                            , Expression'LiteralValue
                                                ( String "string" )
                                            , Expression'LiteralValue
                                                ( String "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" )
                                            , Expression'LiteralValue
                                                ( String "double" )
                                            , Expression'LiteralValue
                                                ( Number "1.234" )
                                            , Expression'LiteralValue
                                                ( String "string" )
                                            , Expression'LiteralValue
                                                ( String "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" )
                                            , Expression'LiteralValue
                                                ( String "int" )
                                            , Expression'LiteralValue
                                                ( Number "1234" )
                                            , Expression'LiteralValue
                                                ( String "string" )
                                            , Expression'LiteralValue
                                                ( String "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" )
                                            , Expression'LiteralValue
                                                ( String "string" )
                                            , Expression'LiteralValue Null
                                            , Expression'LiteralValue
                                                ( String "string" )
                                            , Expression'LiteralValue
                                                ( String "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" )
                                            , Expression'LiteralValue
                                                ( String "double" )
                                            , Expression'LiteralValue
                                                ( Number "1.234" )
                                            ]
                                        }
                                    , filter = Nothing
                                    , over = Nothing
                                    }
                                )
                            , alias = Nothing
                            }
                        ) :| []
                    , from = Nothing
                    , where_ = Nothing
                    , groupBy = Nothing
                    , window = Nothing
                    }
                )
            )
        , orderBy = Nothing
        , limit = Nothing
        }
    )