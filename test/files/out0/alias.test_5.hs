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
                        ( Expression'And
                            ( Expression'And
                                ( Expression'And
                                    ( Expression'And
                                        ( Expression'And
                                            ( Expression'GreaterThan
                                                ( Expression'Column
                                                    ( Namespaced
                                                        { namespace = Nothing
                                                        , value = "y"
                                                        }
                                                    )
                                                )
                                                ( Expression'LiteralValue
                                                    ( Number "0" )
                                                )
                                            )
                                            ( Expression'LessThan
                                                ( Expression'Column
                                                    ( Namespaced
                                                        { namespace = Nothing
                                                        , value = "y"
                                                        }
                                                    )
                                                )
                                                ( Expression'LiteralValue
                                                    ( Number "99" )
                                                )
                                            )
                                        )
                                        ( Expression'NotEquals
                                            ( Expression'Column
                                                ( Namespaced
                                                    { namespace = Nothing
                                                    , value = "y"
                                                    }
                                                )
                                            )
                                            ( Expression'LiteralValue
                                                ( Number "55" )
                                            )
                                        )
                                    )
                                    ( Expression'Not
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
                                                        ( Number "56" )
                                                    , Expression'LiteralValue
                                                        ( Number "57" )
                                                    , Expression'LiteralValue
                                                        ( Number "58" )
                                                    ]
                                                }
                                            )
                                        )
                                    )
                                )
                                ( Expression'Not
                                    ( Expression'Like
                                        ( Expression'Column
                                            ( Namespaced
                                                { namespace = Nothing
                                                , value = "y"
                                                }
                                            )
                                        )
                                        ( Expression'LiteralValue
                                            ( String "abc%" )
                                        ) Nothing
                                    )
                                )
                            )
                            ( Expression'Equals
                                ( Expression'Modulo
                                    ( Expression'Column
                                        ( Namespaced
                                            { namespace = Nothing
                                            , value = "y"
                                            }
                                        )
                                    )
                                    ( Expression'LiteralValue
                                        ( Number "10" )
                                    )
                                )
                                ( Expression'LiteralValue
                                    ( Number "2" )
                                )
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