Statement'Select
    ( SelectStatement
        { commonTableExpressions = Nothing
        , select = CompoundSelect
            ( SelectCore'Select
                ( Select
                    { distinct = False
                    , columns = ResultColumn'Wildcard
                        ( Namespaced
                            { namespace = Nothing
                            , value = ()
                            }
                        ) :| []
                    , from = Just
                        ( Table
                            ( QualifiedTableName
                                { name = Aliased
                                    { value = Namespaced
                                        { namespace = Nothing
                                        , value = "t"
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
                                ( Expression'Equals
                                    ( Expression'Column
                                        ( Namespaced
                                            { namespace = Nothing
                                            , value = "c1"
                                            }
                                        )
                                    )
                                    ( Expression'LiteralValue
                                        ( Number "5" )
                                    )
                                )
                                ( Expression'GreaterThan
                                    ( Expression'Column
                                        ( Namespaced
                                            { namespace = Nothing
                                            , value = "c2"
                                            }
                                        )
                                    )
                                    ( Expression'LiteralValue
                                        ( Number "0" )
                                    )
                                )
                            )
                            ( Expression'LessThanOrEquals
                                ( Expression'Column
                                    ( Namespaced
                                        { namespace = Nothing
                                        , value = "c2"
                                        }
                                    )
                                )
                                ( Expression'LiteralValue
                                    ( Number "5" )
                                )
                            )
                        )
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
                        , value = "c2"
                        }
                    )
                , collation = Nothing
                , ordering = Descending
                , nullsPlacement = NullsLast
                } :| []
            )
        , limit = Nothing
        }
    )