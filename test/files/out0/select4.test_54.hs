Statement'Select
    ( SelectStatement
        { commonTableExpressions = Nothing
        , select = Union
            ( CompoundSelect
                ( SelectCore'Select
                    ( Select
                        { distinct = False
                        , columns = ResultColumn'Expression
                            ( Aliased
                                { value = Expression'LiteralValue
                                    ( Number "0" )
                                , alias = Just "x"
                                }
                            ) :|
                            [ ResultColumn'Expression
                                ( Aliased
                                    { value = Expression'LiteralValue
                                        ( Number "1" )
                                    , alias = Just "y"
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
            )
            ( SelectCore'Select
                ( Select
                    { distinct = False
                    , columns = ResultColumn'Expression
                        ( Aliased
                            { value = Expression'LiteralValue
                                ( Number "2" )
                            , alias = Just "y"
                            }
                        ) :|
                        [ ResultColumn'Expression
                            ( Aliased
                                { value = Expression'Negate
                                    ( Expression'LiteralValue
                                        ( Number "3" )
                                    )
                                , alias = Just "x"
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
        , orderBy = Just
            ( OrderingTerm
                { expression = Expression'Column
                    ( Namespaced
                        { namespace = Nothing
                        , value = "x"
                        }
                    )
                , collation = Nothing
                , ordering = Ascending
                , nullsPlacement = NullsFirst
                } :| []
            )
        , limit = Just
            ( Limit
                { limit = Expression'LiteralValue
                    ( Number "1" )
                , offset = Nothing
                }
            )
        }
    )