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
                                    { namespace = Just
                                        ( Namespaced
                                            { namespace = Nothing
                                            , value = "t2"
                                            }
                                        )
                                    , value = "a"
                                    }
                                )
                            , alias = Nothing
                            }
                        ) :| []
                    , from = Just
                        ( Table'NaturalInnerJoin
                            ( Table
                                ( QualifiedTableName
                                    { name = Aliased
                                        { value = Namespaced
                                            { namespace = Nothing
                                            , value = "t2"
                                            }
                                        , alias = Nothing
                                        }
                                    , indexedBy = Nothing
                                    }
                                )
                            )
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
                        )
                    , where_ = Just
                        ( Expression'LessThanOrEquals
                            ( Expression'Column
                                ( Namespaced
                                    { namespace = Just
                                        ( Namespaced
                                            { namespace = Nothing
                                            , value = "t1"
                                            }
                                        )
                                    , value = "rowid"
                                    }
                                )
                            )
                            ( Expression'LiteralValue
                                ( Number "562949953421312" )
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
                        { namespace = Just
                            ( Namespaced
                                { namespace = Nothing
                                , value = "t1"
                                }
                            )
                        , value = "a"
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