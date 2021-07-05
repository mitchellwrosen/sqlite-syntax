Statement'Select
    ( SelectStatement
        { commonTableExpressions = Nothing
        , select = CompoundSelect
            ( SelectCore'Select
                ( Select
                    { distinct = False
                    , columns = ResultColumn'Expression
                        ( Aliased
                            { value = Expression'FunctionCall
                                ( FunctionCallExpression
                                    { call = FunctionCall
                                        { name = Namespaced
                                            { namespace = Nothing
                                            , value = "count"
                                            }
                                        , arguments = FunctionArguments'Wildcard
                                        }
                                    , filter = Nothing
                                    , over = Nothing
                                    }
                                )
                            , alias = Nothing
                            }
                        ) :| []
                    , from = Just
                        ( Table'InnerJoin
                            ( Table'InnerJoin
                                ( Table'InnerJoin
                                    ( Table'InnerJoin
                                        ( Table'InnerJoin
                                            ( Table'InnerJoin
                                                ( Table'InnerJoin
                                                    ( Table'InnerJoin
                                                        ( Table'InnerJoin
                                                            ( Table
                                                                ( QualifiedTableName
                                                                    { name = Aliased
                                                                        { value = Namespaced
                                                                            { namespace = Nothing
                                                                            , value = "t4"
                                                                            }
                                                                        , alias = Just "x1"
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
                                                                        , alias = Just "x2"
                                                                        }
                                                                    , indexedBy = Nothing
                                                                    }
                                                                )
                                                            )
                                                            ( Just
                                                                ( On
                                                                    ( Expression'Equals
                                                                        ( Expression'Column
                                                                            ( Namespaced
                                                                                { namespace = Just
                                                                                    ( Namespaced
                                                                                        { namespace = Nothing
                                                                                        , value = "x2"
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
                                                                                        , value = "x1"
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
                                                        ( Table
                                                            ( QualifiedTableName
                                                                { name = Aliased
                                                                    { value = Namespaced
                                                                        { namespace = Nothing
                                                                        , value = "t4"
                                                                        }
                                                                    , alias = Just "x3"
                                                                    }
                                                                , indexedBy = Nothing
                                                                }
                                                            )
                                                        )
                                                        ( Just
                                                            ( On
                                                                ( Expression'Equals
                                                                    ( Expression'Column
                                                                        ( Namespaced
                                                                            { namespace = Just
                                                                                ( Namespaced
                                                                                    { namespace = Nothing
                                                                                    , value = "x3"
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
                                                                                    , value = "x2"
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
                                                    ( Table
                                                        ( QualifiedTableName
                                                            { name = Aliased
                                                                { value = Namespaced
                                                                    { namespace = Nothing
                                                                    , value = "t4"
                                                                    }
                                                                , alias = Just "x4"
                                                                }
                                                            , indexedBy = Nothing
                                                            }
                                                        )
                                                    )
                                                    ( Just
                                                        ( On
                                                            ( Expression'Equals
                                                                ( Expression'Column
                                                                    ( Namespaced
                                                                        { namespace = Just
                                                                            ( Namespaced
                                                                                { namespace = Nothing
                                                                                , value = "x4"
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
                                                                                , value = "x3"
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
                                                ( Table
                                                    ( QualifiedTableName
                                                        { name = Aliased
                                                            { value = Namespaced
                                                                { namespace = Nothing
                                                                , value = "t4"
                                                                }
                                                            , alias = Just "x5"
                                                            }
                                                        , indexedBy = Nothing
                                                        }
                                                    )
                                                )
                                                ( Just
                                                    ( On
                                                        ( Expression'Equals
                                                            ( Expression'Column
                                                                ( Namespaced
                                                                    { namespace = Just
                                                                        ( Namespaced
                                                                            { namespace = Nothing
                                                                            , value = "x5"
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
                                                                            , value = "x4"
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
                                            ( Table
                                                ( QualifiedTableName
                                                    { name = Aliased
                                                        { value = Namespaced
                                                            { namespace = Nothing
                                                            , value = "t4"
                                                            }
                                                        , alias = Just "x6"
                                                        }
                                                    , indexedBy = Nothing
                                                    }
                                                )
                                            )
                                            ( Just
                                                ( On
                                                    ( Expression'Equals
                                                        ( Expression'Column
                                                            ( Namespaced
                                                                { namespace = Just
                                                                    ( Namespaced
                                                                        { namespace = Nothing
                                                                        , value = "x6"
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
                                                                        , value = "x5"
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
                                        ( Table
                                            ( QualifiedTableName
                                                { name = Aliased
                                                    { value = Namespaced
                                                        { namespace = Nothing
                                                        , value = "t4"
                                                        }
                                                    , alias = Just "x7"
                                                    }
                                                , indexedBy = Nothing
                                                }
                                            )
                                        )
                                        ( Just
                                            ( On
                                                ( Expression'Equals
                                                    ( Expression'Column
                                                        ( Namespaced
                                                            { namespace = Just
                                                                ( Namespaced
                                                                    { namespace = Nothing
                                                                    , value = "x7"
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
                                                                    , value = "x6"
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
                                    ( Table
                                        ( QualifiedTableName
                                            { name = Aliased
                                                { value = Namespaced
                                                    { namespace = Nothing
                                                    , value = "t4"
                                                    }
                                                , alias = Just "x8"
                                                }
                                            , indexedBy = Nothing
                                            }
                                        )
                                    )
                                    ( Just
                                        ( On
                                            ( Expression'Equals
                                                ( Expression'Column
                                                    ( Namespaced
                                                        { namespace = Just
                                                            ( Namespaced
                                                                { namespace = Nothing
                                                                , value = "x8"
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
                                                                , value = "x7"
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
                                ( Table
                                    ( QualifiedTableName
                                        { name = Aliased
                                            { value = Namespaced
                                                { namespace = Nothing
                                                , value = "t4"
                                                }
                                            , alias = Just "x9"
                                            }
                                        , indexedBy = Nothing
                                        }
                                    )
                                )
                                ( Just
                                    ( On
                                        ( Expression'Equals
                                            ( Expression'Column
                                                ( Namespaced
                                                    { namespace = Just
                                                        ( Namespaced
                                                            { namespace = Nothing
                                                            , value = "x9"
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
                                                            , value = "x8"
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
                            ( Table
                                ( QualifiedTableName
                                    { name = Aliased
                                        { value = Namespaced
                                            { namespace = Nothing
                                            , value = "t4"
                                            }
                                        , alias = Just "x10"
                                        }
                                    , indexedBy = Nothing
                                    }
                                )
                            )
                            ( Just
                                ( On
                                    ( Expression'Equals
                                        ( Expression'Column
                                            ( Namespaced
                                                { namespace = Just
                                                    ( Namespaced
                                                        { namespace = Nothing
                                                        , value = "x10"
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
                                                        , value = "x9"
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
                    , where_ = Nothing
                    , groupBy = Nothing
                    , window = Nothing
                    }
                )
            )
        , orderBy = Nothing
        , limit = Nothing
        }
    )