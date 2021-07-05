Statement'Select
    ( SelectStatement
        { commonTableExpressions = Nothing
        , select = CompoundSelect
            ( SelectCore'Select
                ( Select
                    { distinct = False
                    , columns = ResultColumn'Expression
                        ( Aliased
                            { value = Expression'InValues
                                ( InValuesExpression
                                    { expression = Expression'LiteralValue
                                        ( Blob "0000" )
                                    , values =
                                        [ Expression'FunctionCall
                                            ( FunctionCallExpression
                                                { call = FunctionCall
                                                    { name = Namespaced
                                                        { namespace = Nothing
                                                        , value = "zeroblob"
                                                        }
                                                    , arguments = FunctionArguments'Arguments
                                                        [ Expression'LiteralValue
                                                            ( Number "2" )
                                                        ]
                                                    }
                                                , filter = Nothing
                                                , over = Nothing
                                                }
                                            )
                                        ]
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