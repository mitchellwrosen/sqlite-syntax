Statement'Select
    ( SelectStatement
        { commonTableExpressions = Nothing
        , select = CompoundSelect
            ( SelectCore'Select
                ( Select
                    { distinct = False
                    , columns = ResultColumn'Expression
                        ( Aliased
                            { value = Expression'Subquery
                                ( SelectStatement
                                    { commonTableExpressions = Nothing
                                    , select = UnionAll
                                        ( UnionAll
                                            ( UnionAll
                                                ( UnionAll
                                                    ( UnionAll
                                                        ( UnionAll
                                                            ( UnionAll
                                                                ( UnionAll
                                                                    ( UnionAll
                                                                        ( CompoundSelect
                                                                            ( SelectCore'Select
                                                                                ( Select
                                                                                    { distinct = False
                                                                                    , columns = ResultColumn'Expression
                                                                                        ( Aliased
                                                                                            { value = Expression'LiteralValue
                                                                                                ( Number "1" )
                                                                                            , alias = Nothing
                                                                                            }
                                                                                        ) :| []
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
                                                                                    , where_ = Just
                                                                                        ( Expression'Equals
                                                                                            ( Expression'Column
                                                                                                ( Namespaced
                                                                                                    { namespace = Nothing
                                                                                                    , value = "x"
                                                                                                    }
                                                                                                )
                                                                                            )
                                                                                            ( Expression'LiteralValue
                                                                                                ( Number "0" )
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
                                                                                        { value = Expression'LiteralValue
                                                                                            ( Number "2" )
                                                                                        , alias = Nothing
                                                                                        }
                                                                                    ) :| []
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
                                                                                , where_ = Just
                                                                                    ( Expression'Equals
                                                                                        ( Expression'Column
                                                                                            ( Namespaced
                                                                                                { namespace = Nothing
                                                                                                , value = "x"
                                                                                                }
                                                                                            )
                                                                                        )
                                                                                        ( Expression'Negate
                                                                                            ( Expression'LiteralValue
                                                                                                ( Number "1" )
                                                                                            )
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
                                                                                    { value = Expression'LiteralValue
                                                                                        ( Number "3" )
                                                                                    , alias = Nothing
                                                                                    }
                                                                                ) :| []
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
                                                                            , where_ = Just
                                                                                ( Expression'Equals
                                                                                    ( Expression'Column
                                                                                        ( Namespaced
                                                                                            { namespace = Nothing
                                                                                            , value = "x"
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
                                                                                { value = Expression'LiteralValue
                                                                                    ( Number "4" )
                                                                                , alias = Nothing
                                                                                }
                                                                            ) :| []
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
                                                                        , where_ = Just
                                                                            ( Expression'Equals
                                                                                ( Expression'Column
                                                                                    ( Namespaced
                                                                                        { namespace = Nothing
                                                                                        , value = "x"
                                                                                        }
                                                                                    )
                                                                                )
                                                                                ( Expression'Negate
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
                                                            )
                                                            ( SelectCore'Select
                                                                ( Select
                                                                    { distinct = False
                                                                    , columns = ResultColumn'Expression
                                                                        ( Aliased
                                                                            { value = Expression'LiteralValue
                                                                                ( Number "5" )
                                                                            , alias = Nothing
                                                                            }
                                                                        ) :| []
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
                                                                    , where_ = Just
                                                                        ( Expression'Equals
                                                                            ( Expression'Column
                                                                                ( Namespaced
                                                                                    { namespace = Nothing
                                                                                    , value = "x"
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
                                                                        { value = Expression'LiteralValue
                                                                            ( Number "6" )
                                                                        , alias = Nothing
                                                                        }
                                                                    ) :| []
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
                                                                , where_ = Just
                                                                    ( Expression'Equals
                                                                        ( Expression'Column
                                                                            ( Namespaced
                                                                                { namespace = Nothing
                                                                                , value = "y"
                                                                                }
                                                                            )
                                                                        )
                                                                        ( Expression'LiteralValue
                                                                            ( Number "0" )
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
                                                                    { value = Expression'LiteralValue
                                                                        ( Number "7" )
                                                                    , alias = Nothing
                                                                    }
                                                                ) :| []
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
                                                            , where_ = Just
                                                                ( Expression'Equals
                                                                    ( Expression'Column
                                                                        ( Namespaced
                                                                            { namespace = Nothing
                                                                            , value = "y"
                                                                            }
                                                                        )
                                                                    )
                                                                    ( Expression'LiteralValue
                                                                        ( Number "1" )
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
                                                                { value = Expression'LiteralValue
                                                                    ( Number "8" )
                                                                , alias = Nothing
                                                                }
                                                            ) :| []
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
                                                        , where_ = Just
                                                            ( Expression'Equals
                                                                ( Expression'Column
                                                                    ( Namespaced
                                                                        { namespace = Nothing
                                                                        , value = "y"
                                                                        }
                                                                    )
                                                                )
                                                                ( Expression'Negate
                                                                    ( Expression'LiteralValue
                                                                        ( Number "3" )
                                                                    )
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
                                                            { value = Expression'LiteralValue
                                                                ( Number "9" )
                                                            , alias = Nothing
                                                            }
                                                        ) :| []
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
                                                    , where_ = Just
                                                        ( Expression'Equals
                                                            ( Expression'Column
                                                                ( Namespaced
                                                                    { namespace = Nothing
                                                                    , value = "y"
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
                                                        { value = Expression'LiteralValue
                                                            ( Number "10" )
                                                        , alias = Nothing
                                                        }
                                                    ) :| []
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
                                                , where_ = Just
                                                    ( Expression'Equals
                                                        ( Expression'Column
                                                            ( Namespaced
                                                                { namespace = Nothing
                                                                , value = "y"
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
                                    , orderBy = Nothing
                                    , limit = Nothing
                                    }
                                )
                            , alias = Nothing
                            }
                        ) :| []
                    , from = Nothing
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