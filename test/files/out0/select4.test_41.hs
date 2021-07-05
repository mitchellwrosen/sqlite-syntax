Statement'Select
    ( SelectStatement
        { commonTableExpressions = Nothing
        , select = UnionAll
            ( UnionAll
                ( UnionAll
                    ( CompoundSelect
                        ( SelectCore'Select
                            ( Select
                                { distinct = False
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
                                , where_ = Just
                                    ( Expression'Equals
                                        ( Expression'Column
                                            ( Namespaced
                                                { namespace = Nothing
                                                , value = "n"
                                                }
                                            )
                                        )
                                        ( Expression'LiteralValue
                                            ( Number "2" )
                                        )
                                    )
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
                            , where_ = Just
                                ( Expression'Equals
                                    ( Expression'Column
                                        ( Namespaced
                                            { namespace = Nothing
                                            , value = "n"
                                            }
                                        )
                                    )
                                    ( Expression'LiteralValue
                                        ( Number "3" )
                                    )
                                )
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
                        , where_ = Just
                            ( Expression'Equals
                                ( Expression'Column
                                    ( Namespaced
                                        { namespace = Nothing
                                        , value = "n"
                                        }
                                    )
                                )
                                ( Expression'LiteralValue
                                    ( Number "4" )
                                )
                            )
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
                    , where_ = Just
                        ( Expression'Equals
                            ( Expression'Column
                                ( Namespaced
                                    { namespace = Nothing
                                    , value = "n"
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
        , limit = Nothing
        }
    )