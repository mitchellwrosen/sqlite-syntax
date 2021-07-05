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
                                    , value = "y"
                                    }
                                )
                            , alias = Nothing
                            }
                        ) :|
                        [ ResultColumn'Expression
                            ( Aliased
                                { value = Expression'Column
                                    ( Namespaced
                                        { namespace = Nothing
                                        , value = "p"
                                        }
                                    )
                                , alias = Nothing
                                }
                            )
                        , ResultColumn'Expression
                            ( Aliased
                                { value = Expression'Column
                                    ( Namespaced
                                        { namespace = Nothing
                                        , value = "q"
                                        }
                                    )
                                , alias = Nothing
                                }
                            )
                        , ResultColumn'Expression
                            ( Aliased
                                { value = Expression'Column
                                    ( Namespaced
                                        { namespace = Nothing
                                        , value = "r"
                                        }
                                    )
                                , alias = Nothing
                                }
                            )
                        ]
                    , from = Just
                        ( Table'InnerJoin
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
                                                                    { namespace = Just
                                                                        ( Namespaced
                                                                            { namespace = Nothing
                                                                            , value = "t1"
                                                                            }
                                                                        )
                                                                    , value = "y"
                                                                    }
                                                                )
                                                            , alias = Just "y"
                                                            }
                                                        ) :|
                                                        [ ResultColumn'Expression
                                                            ( Aliased
                                                                { value = Expression'Column
                                                                    ( Namespaced
                                                                        { namespace = Just
                                                                            ( Namespaced
                                                                                { namespace = Nothing
                                                                                , value = "t2"
                                                                                }
                                                                            )
                                                                        , value = "b"
                                                                        }
                                                                    )
                                                                , alias = Just "b"
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
                                                                            , value = "t1"
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
                                                                        , alias = Nothing
                                                                        }
                                                                    , indexedBy = Nothing
                                                                    }
                                                                )
                                                            ) Nothing
                                                        )
                                                    , where_ = Just
                                                        ( Expression'Equals
                                                            ( Expression'Column
                                                                ( Namespaced
                                                                    { namespace = Just
                                                                        ( Namespaced
                                                                            { namespace = Nothing
                                                                            , value = "t1"
                                                                            }
                                                                        )
                                                                    , value = "x"
                                                                    }
                                                                )
                                                            )
                                                            ( Expression'Column
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
                                    , alias = Just "m"
                                    }
                                )
                            )
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
                                                                    { namespace = Just
                                                                        ( Namespaced
                                                                            { namespace = Nothing
                                                                            , value = "t3"
                                                                            }
                                                                        )
                                                                    , value = "p"
                                                                    }
                                                                )
                                                            , alias = Just "p"
                                                            }
                                                        ) :|
                                                        [ ResultColumn'Expression
                                                            ( Aliased
                                                                { value = Expression'Column
                                                                    ( Namespaced
                                                                        { namespace = Just
                                                                            ( Namespaced
                                                                                { namespace = Nothing
                                                                                , value = "t3"
                                                                                }
                                                                            )
                                                                        , value = "q"
                                                                        }
                                                                    )
                                                                , alias = Just "q"
                                                                }
                                                            )
                                                        , ResultColumn'Expression
                                                            ( Aliased
                                                                { value = Expression'Column
                                                                    ( Namespaced
                                                                        { namespace = Just
                                                                            ( Namespaced
                                                                                { namespace = Nothing
                                                                                , value = "t4"
                                                                                }
                                                                            )
                                                                        , value = "r"
                                                                        }
                                                                    )
                                                                , alias = Just "r"
                                                                }
                                                            )
                                                        ]
                                                    , from = Just
                                                        ( Table'NaturalInnerJoin
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
                                                                            , value = "t4"
                                                                            }
                                                                        , alias = Nothing
                                                                        }
                                                                    , indexedBy = Nothing
                                                                    }
                                                                )
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
                                    , alias = Just "n"
                                    }
                                )
                            ) Nothing
                        )
                    , where_ = Just
                        ( Expression'Equals
                            ( Expression'Column
                                ( Namespaced
                                    { namespace = Nothing
                                    , value = "y"
                                    }
                                )
                            )
                            ( Expression'Column
                                ( Namespaced
                                    { namespace = Nothing
                                    , value = "p"
                                    }
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
    )