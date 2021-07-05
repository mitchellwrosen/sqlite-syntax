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
                                            , value = "coalesce"
                                            }
                                        , arguments = FunctionArguments'Arguments
                                            [ Expression'Subquery
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
                                                                                        , value = "max"
                                                                                        }
                                                                                    , arguments = FunctionArguments'Arguments
                                                                                        [ Expression'LiteralValue
                                                                                            ( Number "19" )
                                                                                        ]
                                                                                    }
                                                                                , filter = Nothing
                                                                                , over = Nothing
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
                                                                    ( Expression'Or
                                                                        ( Expression'LessThanOrEquals
                                                                            ( Expression'Multiply
                                                                                ( Expression'Column
                                                                                    ( Namespaced
                                                                                        { namespace = Nothing
                                                                                        , value = "d"
                                                                                        }
                                                                                    )
                                                                                )
                                                                                ( Expression'Negate
                                                                                    ( Expression'Column
                                                                                        ( Namespaced
                                                                                            { namespace = Nothing
                                                                                            , value = "a"
                                                                                            }
                                                                                        )
                                                                                    )
                                                                                )
                                                                            )
                                                                            ( Expression'Minus
                                                                                ( Expression'Minus
                                                                                    ( Expression'Plus
                                                                                        ( Expression'Subquery
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
                                                                                                , orderBy = Nothing
                                                                                                , limit = Nothing
                                                                                                }
                                                                                            )
                                                                                        )
                                                                                        ( Expression'Multiply
                                                                                            ( Expression'Multiply
                                                                                                ( Expression'LiteralValue
                                                                                                    ( Number "19" )
                                                                                                )
                                                                                                ( Expression'LiteralValue
                                                                                                    ( Number "19" )
                                                                                                )
                                                                                            )
                                                                                            ( Expression'Column
                                                                                                ( Namespaced
                                                                                                    { namespace = Nothing
                                                                                                    , value = "a"
                                                                                                    }
                                                                                                )
                                                                                            )
                                                                                        )
                                                                                    )
                                                                                    ( Expression'Column
                                                                                        ( Namespaced
                                                                                            { namespace = Nothing
                                                                                            , value = "e"
                                                                                            }
                                                                                        )
                                                                                    )
                                                                                )
                                                                                ( Expression'Column
                                                                                    ( Namespaced
                                                                                        { namespace = Nothing
                                                                                        , value = "d"
                                                                                        }
                                                                                    )
                                                                                )
                                                                            )
                                                                        )
                                                                        ( Expression'InValues
                                                                            ( InValuesExpression
                                                                                { expression = Expression'Column
                                                                                    ( Namespaced
                                                                                        { namespace = Nothing
                                                                                        , value = "b"
                                                                                        }
                                                                                    )
                                                                                , values =
                                                                                    [ Expression'Plus
                                                                                        ( Expression'Column
                                                                                            ( Namespaced
                                                                                                { namespace = Just
                                                                                                    ( Namespaced
                                                                                                        { namespace = Nothing
                                                                                                        , value = "t1"
                                                                                                        }
                                                                                                    )
                                                                                                , value = "c"
                                                                                                }
                                                                                            )
                                                                                        )
                                                                                        ( Expression'Negate
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
                                                                                        )
                                                                                    , Expression'Column
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
                                                                                    , Expression'Plus
                                                                                        ( Expression'Column
                                                                                            ( Namespaced
                                                                                                { namespace = Nothing
                                                                                                , value = "b"
                                                                                                }
                                                                                            )
                                                                                        )
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
                                                                                    ]
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
                                            , Expression'Column
                                                ( Namespaced
                                                    { namespace = Nothing
                                                    , value = "d"
                                                    }
                                                )
                                            ]
                                        }
                                    , filter = Nothing
                                    , over = Nothing
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
                        ( Expression'Not
                            ( Expression'Exists
                                ( SelectStatement
                                    { commonTableExpressions = Nothing
                                    , select = CompoundSelect
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
                                                                    , value = "t1"
                                                                    }
                                                                , alias = Nothing
                                                                }
                                                            , indexedBy = Nothing
                                                            }
                                                        )
                                                    )
                                                , where_ = Just
                                                    ( Expression'Exists
                                                        ( SelectStatement
                                                            { commonTableExpressions = Nothing
                                                            , select = CompoundSelect
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
                                                                                            , value = "t1"
                                                                                            }
                                                                                        , alias = Nothing
                                                                                        }
                                                                                    , indexedBy = Nothing
                                                                                    }
                                                                                )
                                                                            )
                                                                        , where_ = Just
                                                                            ( Expression'Not
                                                                                ( Expression'GreaterThan
                                                                                    ( Expression'Column
                                                                                        ( Namespaced
                                                                                            { namespace = Just
                                                                                                ( Namespaced
                                                                                                    { namespace = Nothing
                                                                                                    , value = "t1"
                                                                                                    }
                                                                                                )
                                                                                            , value = "e"
                                                                                            }
                                                                                        )
                                                                                    )
                                                                                    ( Expression'Column
                                                                                        ( Namespaced
                                                                                            { namespace = Nothing
                                                                                            , value = "e"
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
                                                            , orderBy = Nothing
                                                            , limit = Nothing
                                                            }
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