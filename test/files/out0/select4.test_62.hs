Statement'Select
    ( SelectStatement
        { commonTableExpressions = Nothing
        , select = CompoundSelect
            ( SelectCore'Select
                ( Select
                    { distinct = True
                    , columns = ResultColumn'Expression
                        ( Aliased
                            { value = Expression'Column
                                ( Namespaced
                                    { namespace = Nothing
                                    , value = "log"
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
                        , value = "log"
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
                    ( Number "0" )
                , offset = Just
                    ( Expression'LiteralValue
                        ( Number "3" )
                    )
                }
            )
        }
    )