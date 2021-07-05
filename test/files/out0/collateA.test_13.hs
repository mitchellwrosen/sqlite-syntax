Statement'Select
    ( SelectStatement
        { commonTableExpressions = Nothing
        , select = CompoundSelect
            ( SelectCore'Select
                ( Select
                    { distinct = False
                    , columns = ResultColumn'Expression
                        ( Aliased
                            { value = Expression'Equals
                                ( Expression'LiteralValue
                                    ( String "  " )
                                )
                                ( Expression'Collate
                                    ( CollateExpression
                                        { expression = Expression'LiteralValue
                                            ( String "" )
                                        , collation = "RTRIM"
                                        }
                                    )
                                )
                            , alias = Nothing
                            }
                        ) :|
                        [ ResultColumn'Expression
                            ( Aliased
                                { value = Expression'Equals
                                    ( Expression'LiteralValue
                                        ( String "  " )
                                    )
                                    ( Expression'Collate
                                        ( CollateExpression
                                            { expression = Expression'LiteralValue
                                                ( String "" )
                                            , collation = "BINARY"
                                            }
                                        )
                                    )
                                , alias = Nothing
                                }
                            )
                        , ResultColumn'Expression
                            ( Aliased
                                { value = Expression'Equals
                                    ( Expression'LiteralValue
                                        ( String "  " )
                                    )
                                    ( Expression'LiteralValue
                                        ( String "" )
                                    )
                                , alias = Nothing
                                }
                            )
                        ]
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