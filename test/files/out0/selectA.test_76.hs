Statement'Select
    ( SelectStatement
        { commonTableExpressions = Nothing
        , select = Except
            ( Intersect
                ( Except
                    ( Intersect
                        ( Except
                            ( Intersect
                                ( Except
                                    ( Intersect
                                        ( Intersect
                                            ( CompoundSelect
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
                                                            ) :|
                                                            [ ResultColumn'Expression
                                                                ( Aliased
                                                                    { value = Expression'Column
                                                                        ( Namespaced
                                                                            { namespace = Nothing
                                                                            , value = "b"
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
                                                                            , value = "c"
                                                                            }
                                                                        )
                                                                    , alias = Nothing
                                                                    }
                                                                )
                                                            ]
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
                                                                        , value = "b"
                                                                        }
                                                                    )
                                                                )
                                                                ( Expression'LiteralValue
                                                                    ( String "d" )
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
                                                                    , value = "a"
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
                                                                        , value = "b"
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
                                                                        , value = "c"
                                                                        }
                                                                    )
                                                                , alias = Nothing
                                                                }
                                                            )
                                                        ]
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
                                        )
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
                                                    ) :|
                                                    [ ResultColumn'Expression
                                                        ( Aliased
                                                            { value = Expression'Column
                                                                ( Namespaced
                                                                    { namespace = Nothing
                                                                    , value = "b"
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
                                                                    , value = "c"
                                                                    }
                                                                )
                                                            , alias = Nothing
                                                            }
                                                        )
                                                    ]
                                                , from = Just
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
                                                    { value = Expression'Column
                                                        ( Namespaced
                                                            { namespace = Nothing
                                                            , value = "b"
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
                                                                , value = "c"
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
                                                                , value = "a"
                                                                }
                                                            )
                                                        , alias = Nothing
                                                        }
                                                    )
                                                ]
                                            , from = Just
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
                                                { value = Expression'Column
                                                    ( Namespaced
                                                        { namespace = Nothing
                                                        , value = "a"
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
                                                            , value = "b"
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
                                                            , value = "c"
                                                            }
                                                        )
                                                    , alias = Nothing
                                                    }
                                                )
                                            ]
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
                            )
                            ( SelectCore'Select
                                ( Select
                                    { distinct = False
                                    , columns = ResultColumn'Expression
                                        ( Aliased
                                            { value = Expression'Column
                                                ( Namespaced
                                                    { namespace = Nothing
                                                    , value = "x"
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
                                                        , value = "y"
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
                                                        , value = "z"
                                                        }
                                                    )
                                                , alias = Nothing
                                                }
                                            )
                                        ]
                                    , from = Just
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
                                        { value = Expression'Column
                                            ( Namespaced
                                                { namespace = Nothing
                                                , value = "a"
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
                                                    , value = "b"
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
                                                    , value = "c"
                                                    }
                                                )
                                            , alias = Nothing
                                            }
                                        )
                                    ]
                                , from = Just
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
                                                , value = "x"
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
                                                , value = "z"
                                                }
                                            )
                                        , alias = Nothing
                                        }
                                    )
                                ]
                            , from = Just
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
                                { value = Expression'Column
                                    ( Namespaced
                                        { namespace = Nothing
                                        , value = "a"
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
                                            , value = "b"
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
                                            , value = "c"
                                            }
                                        )
                                    , alias = Nothing
                                    }
                                )
                            ]
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
            )
            ( SelectCore'Select
                ( Select
                    { distinct = False
                    , columns = ResultColumn'Expression
                        ( Aliased
                            { value = Expression'Column
                                ( Namespaced
                                    { namespace = Nothing
                                    , value = "c"
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
                                        , value = "b"
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
                                        , value = "a"
                                        }
                                    )
                                , alias = Nothing
                                }
                            )
                        ]
                    , from = Just
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
                        , value = "c"
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