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
                                    , value = "c9"
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
                        , value = "c1"
                        }
                    )
                , collation = Nothing
                , ordering = Ascending
                , nullsPlacement = NullsFirst
                } :|
                [ OrderingTerm
                    { expression = Expression'Column
                        ( Namespaced
                            { namespace = Nothing
                            , value = "c2"
                            }
                        )
                    , collation = Nothing
                    , ordering = Ascending
                    , nullsPlacement = NullsFirst
                    }
                , OrderingTerm
                    { expression = Expression'Column
                        ( Namespaced
                            { namespace = Nothing
                            , value = "c3"
                            }
                        )
                    , collation = Nothing
                    , ordering = Ascending
                    , nullsPlacement = NullsFirst
                    }
                , OrderingTerm
                    { expression = Expression'Column
                        ( Namespaced
                            { namespace = Nothing
                            , value = "c4"
                            }
                        )
                    , collation = Nothing
                    , ordering = Ascending
                    , nullsPlacement = NullsFirst
                    }
                , OrderingTerm
                    { expression = Expression'Column
                        ( Namespaced
                            { namespace = Nothing
                            , value = "c5"
                            }
                        )
                    , collation = Nothing
                    , ordering = Ascending
                    , nullsPlacement = NullsFirst
                    }
                , OrderingTerm
                    { expression = Expression'Column
                        ( Namespaced
                            { namespace = Nothing
                            , value = "c6"
                            }
                        )
                    , collation = Nothing
                    , ordering = Ascending
                    , nullsPlacement = NullsFirst
                    }
                ]
            )
        , limit = Just
            ( Limit
                { limit = Expression'LiteralValue
                    ( Number "5" )
                , offset = Nothing
                }
            )
        }
    )