Statement'Select
    ( SelectStatement
        { commonTableExpressions = Nothing
        , select = CompoundSelect
            ( SelectCore'Select
                ( Select
                    { distinct = False
                    , columns = ResultColumn'Expression
                        ( Aliased
                            { value = Expression'Column
                                ( Namespaced
                                    { namespace = Nothing
                                    , value = "a"
                                    }
                                )
                            , alias = Nothing
                            }
                        ) :| []
                    , from = Just
                        ( Table
                            ( QualifiedTableName
                                { name = Aliased
                                    { value = Namespaced
                                        { namespace = Nothing
                                        , value = "t1"
                                        }
                                    , alias = Nothing
                                    }
                                , indexedBy = Nothing
                                }
                            )
                        )
                    , where_ = Just
                        ( Expression'FunctionCall
                            ( FunctionCallExpression
                                { call = FunctionCall
                                    { name = Namespaced
                                        { namespace = Nothing
                                        , value = "min"
                                        }
                                    , arguments = FunctionArguments'Arguments
                                        [ Expression'LiteralValue
                                            ( Number "0" )
                                        , Expression'InValues
                                            ( InValuesExpression
                                                { expression = Expression'Column
                                                    ( Namespaced
                                                        { namespace = Nothing
                                                        , value = "b"
                                                        }
                                                    )
                                                , values =
                                                    [ Expression'Column
                                                        ( Namespaced
                                                            { namespace = Nothing
                                                            , value = "a"
                                                            }
                                                        )
                                                    , Expression'LiteralValue
                                                        ( Number "30" )
                                                    ]
                                                }
                                            )
                                        ]
                                    }
                                , filter = Nothing
                                , over = Nothing
                                }
                            )
                        )
                    , groupBy = Nothing
                    , window = Nothing
                    }
                )
            )
        , orderBy = Nothing
        , limit = Nothing
        }
    )