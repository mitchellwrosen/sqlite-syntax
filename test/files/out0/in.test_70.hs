Statement'Select
    ( SelectStatement
        { commonTableExpressions = Nothing
        , select = CompoundSelect
            ( SelectCore'Select
                ( Select
                    { distinct = False
                    , columns = ResultColumn'Expression
                        ( Aliased
                            { value = Expression'Not
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
                                                                        { namespace = Just
                                                                            ( Namespaced
                                                                                { namespace = Nothing
                                                                                , value = "inside"
                                                                                }
                                                                            )
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
                                                                            , value = "t7"
                                                                            }
                                                                        , alias = Just "inside"
                                                                        }
                                                                    , indexedBy = Nothing
                                                                    }
                                                                )
                                                            )
                                                        , where_ = Just
                                                            ( Expression'Between
                                                                ( Expression'Column
                                                                    ( Namespaced
                                                                        { namespace = Just
                                                                            ( Namespaced
                                                                                { namespace = Nothing
                                                                                , value = "inside"
                                                                                }
                                                                            )
                                                                        , value = "b"
                                                                        }
                                                                    )
                                                                )
                                                                ( Expression'Plus
                                                                    ( Expression'Column
                                                                        ( Namespaced
                                                                            { namespace = Just
                                                                                ( Namespaced
                                                                                    { namespace = Nothing
                                                                                    , value = "outside"
                                                                                    }
                                                                                )
                                                                            , value = "b"
                                                                            }
                                                                        )
                                                                    )
                                                                    ( Expression'LiteralValue
                                                                        ( Number "1" )
                                                                    )
                                                                )
                                                                ( Expression'Plus
                                                                    ( Expression'Column
                                                                        ( Namespaced
                                                                            { namespace = Just
                                                                                ( Namespaced
                                                                                    { namespace = Nothing
                                                                                    , value = "outside"
                                                                                    }
                                                                                )
                                                                            , value = "b"
                                                                            }
                                                                        )
                                                                    )
                                                                    ( Expression'LiteralValue
                                                                        ( Number "2" )
                                                                    )
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
                            , alias = Nothing
                            }
                        ) :| []
                    , from = Just
                        ( Table
                            ( QualifiedTableName
                                { name = Aliased
                                    { value = Namespaced
                                        { namespace = Nothing
                                        , value = "t7"
                                        }
                                    , alias = Just "outside"
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