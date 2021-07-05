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
                                    , value = "z"
                                    }
                                )
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
                                                        { value = Expression'Plus
                                                            ( Expression'Multiply
                                                                ( Expression'Column
                                                                    ( Namespaced
                                                                        { namespace = Nothing
                                                                        , value = "y"
                                                                        }
                                                                    )
                                                                )
                                                                ( Expression'LiteralValue
                                                                    ( Number "10" )
                                                                )
                                                            )
                                                            ( Expression'Column
                                                                ( Namespaced
                                                                    { namespace = Nothing
                                                                    , value = "x"
                                                                    }
                                                                )
                                                            )
                                                        , alias = Just "z"
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
                                                ( Number "10" )
                                            , offset = Nothing
                                            }
                                        )
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
                { expression = Expression'Column
                    ( Namespaced
                        { namespace = Nothing
                        , value = "z"
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
                    ( Number "5" )
                , offset = Nothing
                }
            )
        }
    )