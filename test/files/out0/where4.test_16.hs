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
                                    , value = "rowid"
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
                                        , value = "t5"
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
                                    ( Expression'InValues
                                        ( InValuesExpression
                                            { expression = Expression'Column
                                                ( Namespaced
                                                    { namespace = Nothing
                                                    , value = "a"
                                                    }
                                                )
                                            , values =
                                                [ Expression'LiteralValue
                                                    ( Number "1" )
                                                , Expression'LiteralValue
                                                    ( Number "9" )
                                                , Expression'LiteralValue
                                                    ( Number "2" )
                                                ]
                                            }
                                        )
                                    )
                                    ( Expression'Equals
                                        ( Expression'Column
                                            ( Namespaced
                                                { namespace = Nothing
                                                , value = "b"
                                                }
                                            )
                                        )
                                        ( Expression'LiteralValue
                                            ( Number "2" )
                                        )
                                    )
                                )
                                ( Expression'InValues
                                    ( InValuesExpression
                                        { expression = Expression'Column
                                            ( Namespaced
                                                { namespace = Nothing
                                                , value = "c"
                                                }
                                            )
                                        , values =
                                            [ Expression'LiteralValue
                                                ( Number "1" )
                                            , Expression'LiteralValue
                                                ( Number "2" )
                                            , Expression'LiteralValue
                                                ( Number "3" )
                                            , Expression'LiteralValue
                                                ( Number "4" )
                                            ]
                                        }
                                    )
                                )
                            )
                            ( Expression'GreaterThan
                                ( Expression'Column
                                    ( Namespaced
                                        { namespace = Nothing
                                        , value = "d"
                                        }
                                    )
                                )
                                ( Expression'LiteralValue
                                    ( Number "0" )
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