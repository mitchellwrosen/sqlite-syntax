Statement'Select
    ( SelectStatement
        { commonTableExpressions = Nothing
        , select = CompoundSelect
            ( SelectCore'Select
                ( Select
                    { distinct = False
                    , columns = ResultColumn'Expression
                        ( Aliased
                            { value = Expression'Concatenate
                                ( Expression'Concatenate
                                    ( Expression'Concatenate
                                        ( Expression'Concatenate
                                            ( Expression'Column
                                                ( Namespaced
                                                    { namespace = Just
                                                        ( Namespaced
                                                            { namespace = Nothing
                                                            , value = "x"
                                                            }
                                                        )
                                                    , value = "a"
                                                    }
                                                )
                                            )
                                            ( Expression'LiteralValue
                                                ( String "/" )
                                            )
                                        )
                                        ( Expression'Column
                                            ( Namespaced
                                                { namespace = Just
                                                    ( Namespaced
                                                        { namespace = Nothing
                                                        , value = "x"
                                                        }
                                                    )
                                                , value = "b"
                                                }
                                            )
                                        )
                                    )
                                    ( Expression'LiteralValue
                                        ( String "/" )
                                    )
                                )
                                ( Expression'Column
                                    ( Namespaced
                                        { namespace = Just
                                            ( Namespaced
                                                { namespace = Nothing
                                                , value = "y"
                                                }
                                            )
                                        , value = "b"
                                        }
                                    )
                                )
                            , alias = Nothing
                            }
                        ) :| []
                    , from = Just
                        ( Table'InnerJoin
                            ( Table
                                ( QualifiedTableName
                                    { name = Aliased
                                        { value = Namespaced
                                            { namespace = Nothing
                                            , value = "v1"
                                            }
                                        , alias = Just "x"
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
                                            , value = "v1"
                                            }
                                        , alias = Just "y"
                                        }
                                    , indexedBy = Nothing
                                    }
                                )
                            )
                            ( Just
                                ( On
                                    ( Expression'And
                                        ( Expression'Equals
                                            ( Expression'Column
                                                ( Namespaced
                                                    { namespace = Just
                                                        ( Namespaced
                                                            { namespace = Nothing
                                                            , value = "x"
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
                                                            , value = "y"
                                                            }
                                                        )
                                                    , value = "a"
                                                    }
                                                )
                                            )
                                        )
                                        ( Expression'LessThan
                                            ( Expression'Column
                                                ( Namespaced
                                                    { namespace = Just
                                                        ( Namespaced
                                                            { namespace = Nothing
                                                            , value = "x"
                                                            }
                                                        )
                                                    , value = "b"
                                                    }
                                                )
                                            )
                                            ( Expression'Column
                                                ( Namespaced
                                                    { namespace = Just
                                                        ( Namespaced
                                                            { namespace = Nothing
                                                            , value = "y"
                                                            }
                                                        )
                                                    , value = "b"
                                                    }
                                                )
                                            )
                                        )
                                    )
                                )
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
                        { namespace = Just
                            ( Namespaced
                                { namespace = Nothing
                                , value = "x"
                                }
                            )
                        , value = "a"
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
                                    , value = "x"
                                    }
                                )
                            , value = "b"
                            }
                        )
                    , collation = Nothing
                    , ordering = Ascending
                    , nullsPlacement = NullsFirst
                    }
                , OrderingTerm
                    { expression = Expression'Column
                        ( Namespaced
                            { namespace = Just
                                ( Namespaced
                                    { namespace = Nothing
                                    , value = "y"
                                    }
                                )
                            , value = "b"
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