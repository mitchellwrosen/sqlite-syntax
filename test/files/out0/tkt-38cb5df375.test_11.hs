Statement'Select
    ( SelectStatement
        { commonTableExpressions = Nothing
        , select = UnionAll
            ( CompoundSelect
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
                            ( Table'Subquery
                                ( Aliased
                                    { value = SelectStatement
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
                                                        , value = "a"
                                                        }
                                                    )
                                                , collation = Nothing
                                                , ordering = Ascending
                                                , nullsPlacement = NullsFirst
                                                } :| []
                                            )
                                        , limit = Nothing
                                        }
                                    , alias = Nothing
                                    }
                                )
                            )
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
                                ( Number "9" )
                            , alias = Nothing
                            }
                        ) :| []
                    , from = Just
                        ( Table'Subquery
                            ( Aliased
                                { value = SelectStatement
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
                                    , orderBy = Nothing
                                    , limit = Nothing
                                    }
                                , alias = Nothing
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
                { expression = Expression'LiteralValue
                    ( Number "1" )
                , collation = Nothing
                , ordering = Ascending
                , nullsPlacement = NullsFirst
                } :| []
            )
        , limit = Just
            ( Limit
                { limit = Expression'Column
                    ( Namespaced
                        { namespace = Nothing
                        , value = "ii"
                        }
                    )
                , offset = Nothing
                }
            )
        }
    )