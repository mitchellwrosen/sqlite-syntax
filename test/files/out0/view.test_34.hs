Statement'Select
    ( SelectStatement
        { commonTableExpressions = Nothing
        , select = CompoundSelect
            ( SelectCore'Select
                ( Select
                    { distinct = False
                    , columns = ResultColumn'Expression
                        ( Aliased
                            { value = Expression'Plus
                                ( Expression'Column
                                    ( Namespaced
                                        { namespace = Nothing
                                        , value = "mx"
                                        }
                                    )
                                )
                                ( Expression'LiteralValue
                                    ( Number "10" )
                                )
                            , alias = Nothing
                            }
                        ) :|
                        [ ResultColumn'Expression
                            ( Aliased
                                { value = Expression'Multiply
                                    ( Expression'Column
                                        ( Namespaced
                                            { namespace = Nothing
                                            , value = "mx"
                                            }
                                        )
                                    )
                                    ( Expression'LiteralValue
                                        ( Number "2" )
                                    )
                                , alias = Nothing
                                }
                            )
                        ]
                    , from = Just
                        ( Table
                            ( QualifiedTableName
                                { name = Aliased
                                    { value = Namespaced
                                        { namespace = Nothing
                                        , value = "v8"
                                        }
                                    , alias = Nothing
                                    }
                                , indexedBy = Nothing
                                }
                            )
                        )
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