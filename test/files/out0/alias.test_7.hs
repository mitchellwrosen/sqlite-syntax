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
                                    , value = "x"
                                    }
                                )
                            , alias = Nothing
                            }
                        ) :|
                        [ ResultColumn'Expression
                            ( Aliased
                                { value = Expression'FunctionCall
                                    ( FunctionCallExpression
                                        { call = FunctionCall
                                            { name = Namespaced
                                                { namespace = Nothing
                                                , value = "sequence"
                                                }
                                            , arguments = FunctionArguments'Arguments []
                                            }
                                        , filter = Nothing
                                        , over = Nothing
                                        }
                                    )
                                , alias = Just "y"
                                }
                            )
                        ]
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
                        ( Expression'InValues
                            ( InValuesExpression
                                { expression = Expression'Column
                                    ( Namespaced
                                        { namespace = Nothing
                                        , value = "y"
                                        }
                                    )
                                , values =
                                    [ Expression'LiteralValue
                                        ( Number "55" )
                                    , Expression'LiteralValue
                                        ( Number "66" )
                                    , Expression'LiteralValue
                                        ( Number "3" )
                                    ]
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