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
                            , alias = Just "x"
                            }
                        ) :|
                        [ ResultColumn'Expression
                            ( Aliased
                                { value = Expression'Concatenate
                                    ( Expression'Column
                                        ( Namespaced
                                            { namespace = Nothing
                                            , value = "b"
                                            }
                                        )
                                    )
                                    ( Expression'Column
                                        ( Namespaced
                                            { namespace = Nothing
                                            , value = "c"
                                            }
                                        )
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
                        ( Expression'Equals
                            ( Expression'Column
                                ( Namespaced
                                    { namespace = Nothing
                                    , value = "y"
                                    }
                                )
                            )
                            ( Expression'LiteralValue
                                ( String "aaabbb" )
                            )
                        )
                    , groupBy = Just
                        ( GroupBy
                            { groupBy = Expression'Column
                                ( Namespaced
                                    { namespace = Nothing
                                    , value = "x"
                                    }
                                ) :|
                                [ Expression'Column
                                    ( Namespaced
                                        { namespace = Nothing
                                        , value = "y"
                                        }
                                    )
                                ]
                            , having = Nothing
                            }
                        )
                    , window = Nothing
                    }
                )
            )
        , orderBy = Nothing
        , limit = Nothing
        }
    )