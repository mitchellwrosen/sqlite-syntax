Statement'Select
    ( SelectStatement
        { commonTableExpressions = Nothing
        , select = CompoundSelect
            ( SelectCore'Select
                ( Select
                    { distinct = False
                    , columns = ResultColumn'Expression
                        ( Aliased
                            { value = Expression'Minus
                                ( Expression'Column
                                    ( Namespaced
                                        { namespace = Nothing
                                        , value = "f1"
                                        }
                                    )
                                )
                                ( Expression'LiteralValue
                                    ( Number "22" )
                                )
                            , alias = Just "x"
                            }
                        ) :|
                        [ ResultColumn'Expression
                            ( Aliased
                                { value = Expression'Minus
                                    ( Expression'Column
                                        ( Namespaced
                                            { namespace = Nothing
                                            , value = "f2"
                                            }
                                        )
                                    )
                                    ( Expression'LiteralValue
                                        ( Number "22" )
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
                                        , value = "test1"
                                        }
                                    , alias = Nothing
                                    }
                                , indexedBy = Nothing
                                }
                            )
                        )
                    , where_ = Just
                        ( Expression'And
                            ( Expression'GreaterThan
                                ( Expression'Column
                                    ( Namespaced
                                        { namespace = Nothing
                                        , value = "x"
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
                                    ( Number "50" )
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