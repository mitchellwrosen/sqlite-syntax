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
                                            , value = "A"
                                            }
                                        )
                                    , value = "f1"
                                    }
                                )
                            , alias = Nothing
                            }
                        ) :|
                        [ ResultColumn'Expression
                            ( Aliased
                                { value = Expression'Column
                                    ( Namespaced
                                        { namespace = Just
                                            ( Namespaced
                                                { namespace = Nothing
                                                , value = "B"
                                                }
                                            )
                                        , value = "f1"
                                        }
                                    )
                                , alias = Nothing
                                }
                            )
                        ]
                    , from = Just
                        ( Table'InnerJoin
                            ( Table
                                ( QualifiedTableName
                                    { name = Aliased
                                        { value = Namespaced
                                            { namespace = Nothing
                                            , value = "test1"
                                            }
                                        , alias = Just "A"
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
                                            , value = "test1"
                                            }
                                        , alias = Just "B"
                                        }
                                    , indexedBy = Nothing
                                    }
                                )
                            ) Nothing
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
                        { namespace = Just
                            ( Namespaced
                                { namespace = Nothing
                                , value = "A"
                                }
                            )
                        , value = "f1"
                        }
                    )
                , collation = Nothing
                , ordering = Ascending
                , nullsPlacement = NullsFirst
                } :|
                [ OrderingTerm
                    { expression = Expression'Column
                        ( Namespaced
                            { namespace = Just
                                ( Namespaced
                                    { namespace = Nothing
                                    , value = "B"
                                    }
                                )
                            , value = "f1"
                            }
                        )
                    , collation = Nothing
                    , ordering = Ascending
                    , nullsPlacement = NullsFirst
                    }
                ]
            )
        , limit = Nothing
        }
    )