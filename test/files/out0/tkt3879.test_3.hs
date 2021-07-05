Statement'Select
    ( SelectStatement
        { commonTableExpressions = Nothing
        , select = CompoundSelect
            ( SelectCore'Select
                ( Select
                    { distinct = False
                    , columns = ResultColumn'Expression
                        ( Aliased
                            { value = Expression'LiteralValue
                                ( Number "222" )
                            , alias = Nothing
                            }
                        ) :|
                        [ ResultColumn'Expression
                            ( Aliased
                                { value = Expression'Multiply
                                    ( Expression'Column
                                        ( Namespaced
                                            { namespace = Just
                                                ( Namespaced
                                                    { namespace = Nothing
                                                    , value = "t1"
                                                    }
                                                )
                                            , value = "b"
                                            }
                                        )
                                    )
                                    ( Expression'LiteralValue
                                        ( Number "123" )
                                    )
                                , alias = Nothing
                                }
                            )
                        ]
                    , from = Just
                        ( Table'InnerJoin
                            ( Table'InnerJoin
                                ( Table'InnerJoin
                                    ( Table
                                        ( QualifiedTableName
                                            { name = Aliased
                                                { value = Namespaced
                                                    { namespace = Nothing
                                                    , value = "t3"
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
                                                    , value = "t2"
                                                    }
                                                , alias = Just "j0"
                                                }
                                            , indexedBy = Nothing
                                            }
                                        )
                                    ) Nothing
                                )
                                ( Table
                                    ( QualifiedTableName
                                        { name = Aliased
                                            { value = Namespaced
                                                { namespace = Nothing
                                                , value = "t2"
                                                }
                                            , alias = Just "j1"
                                            }
                                        , indexedBy = Nothing
                                        }
                                    )
                                ) Nothing
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
                            ) Nothing
                        )
                    , where_ = Just
                        ( Expression'And
                            ( Expression'And
                                ( Expression'Equals
                                    ( Expression'Column
                                        ( Namespaced
                                            { namespace = Just
                                                ( Namespaced
                                                    { namespace = Nothing
                                                    , value = "j0"
                                                    }
                                                )
                                            , value = "m"
                                            }
                                        )
                                    )
                                    ( Expression'Column
                                        ( Namespaced
                                            { namespace = Just
                                                ( Namespaced
                                                    { namespace = Nothing
                                                    , value = "t3"
                                                    }
                                                )
                                            , value = "m"
                                            }
                                        )
                                    )
                                )
                                ( Expression'Equals
                                    ( Expression'Column
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
                                    )
                                    ( Expression'Column
                                        ( Namespaced
                                            { namespace = Just
                                                ( Namespaced
                                                    { namespace = Nothing
                                                    , value = "j0"
                                                    }
                                                )
                                            , value = "a"
                                            }
                                        )
                                    )
                                )
                            )
                            ( Expression'Equals
                                ( Expression'Column
                                    ( Namespaced
                                        { namespace = Just
                                            ( Namespaced
                                                { namespace = Nothing
                                                , value = "j1"
                                                }
                                            )
                                        , value = "n"
                                        }
                                    )
                                )
                                ( Expression'Column
                                    ( Namespaced
                                        { namespace = Just
                                            ( Namespaced
                                                { namespace = Nothing
                                                , value = "j0"
                                                }
                                            )
                                        , value = "m"
                                        }
                                    )
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
                        { namespace = Just
                            ( Namespaced
                                { namespace = Nothing
                                , value = "t1"
                                }
                            )
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