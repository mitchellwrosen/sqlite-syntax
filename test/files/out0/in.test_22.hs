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
                                        , value = "a"
                                        }
                                    )
                                )
                                ( Expression'Multiply
                                    ( Expression'LiteralValue
                                        ( Number "100" )
                                    )
                                    ( Expression'InSubquery
                                        ( InSubqueryExpression
                                            { expression = Expression'Column
                                                ( Namespaced
                                                    { namespace = Nothing
                                                    , value = "b"
                                                    }
                                                )
                                            , subquery = SelectStatement
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
                                                                            , value = "b"
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
                                                            , where_ = Just
                                                                ( Expression'LessThan
                                                                    ( Expression'Column
                                                                        ( Namespaced
                                                                            { namespace = Nothing
                                                                            , value = "a"
                                                                            }
                                                                        )
                                                                    )
                                                                    ( Expression'LiteralValue
                                                                        ( Number "5" )
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
                                            }
                                        )
                                    )
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
                        , value = "b"
                        }
                    )
                , collation = Nothing
                , ordering = Ascending
                , nullsPlacement = NullsFirst
                } :| []
            )
        , limit = Nothing
        }
    )