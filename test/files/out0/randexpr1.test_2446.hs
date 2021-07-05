Statement'Select
    ( SelectStatement
        { commonTableExpressions = Nothing
        , select = CompoundSelect
            ( SelectCore'Select
                ( Select
                    { distinct = False
                    , columns = ResultColumn'Expression
                        ( Aliased
                            { value = Expression'BitwiseOr
                                ( Expression'Minus
                                    ( Expression'Plus
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
                                    )
                                    ( Expression'FunctionCall
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
                                                                                                [ Expression'Divide
                                                                                                    ( Expression'FunctionCall
                                                                                                        ( FunctionCallExpression
                                                                                                            { call = FunctionCall
                                                                                                                { name = Namespaced
                                                                                                                    { namespace = Nothing
                                                                                                                    , value = "abs"
                                                                                                                    }
                                                                                                                , arguments = FunctionArguments'Arguments
                                                                                                                    [ Expression'Plus
                                                                                                                        ( Expression'FunctionCall
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
                                                                                                                                                                    { value = Expression'Case
                                                                                                                                                                        ( CaseExpression
                                                                                                                                                                            { base = Nothing
                                                                                                                                                                            , cases =
                                                                                                                                                                                ( Expression'GreaterThanOrEquals
                                                                                                                                                                                    ( Expression'Column
                                                                                                                                                                                        ( Namespaced
                                                                                                                                                                                            { namespace = Nothing
                                                                                                                                                                                            , value = "f"
                                                                                                                                                                                            }
                                                                                                                                                                                        )
                                                                                                                                                                                    )
                                                                                                                                                                                    ( Expression'Plus
                                                                                                                                                                                        ( Expression'Divide
                                                                                                                                                                                            ( Expression'FunctionCall
                                                                                                                                                                                                ( FunctionCallExpression
                                                                                                                                                                                                    { call = FunctionCall
                                                                                                                                                                                                        { name = Namespaced
                                                                                                                                                                                                            { namespace = Nothing
                                                                                                                                                                                                            , value = "abs"
                                                                                                                                                                                                            }
                                                                                                                                                                                                        , arguments = FunctionArguments'Arguments
                                                                                                                                                                                                            [ Expression'FunctionCall
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
                                                                                                                                                                                                                                                                        [ Expression'Column
                                                                                                                                                                                                                                                                            ( Namespaced
                                                                                                                                                                                                                                                                                { namespace = Nothing
                                                                                                                                                                                                                                                                                , value = "a"
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
                                                                                                                                                                                                                                                    ( Expression'Equals
                                                                                                                                                                                                                                                        ( Expression'Multiply
                                                                                                                                                                                                                                                            ( Expression'Multiply
                                                                                                                                                                                                                                                                ( Expression'Multiply
                                                                                                                                                                                                                                                                    ( Expression'LiteralValue
                                                                                                                                                                                                                                                                        ( Number "11" )
                                                                                                                                                                                                                                                                    )
                                                                                                                                                                                                                                                                    ( Expression'Column
                                                                                                                                                                                                                                                                        ( Namespaced
                                                                                                                                                                                                                                                                            { namespace = Just
                                                                                                                                                                                                                                                                                ( Namespaced
                                                                                                                                                                                                                                                                                    { namespace = Nothing
                                                                                                                                                                                                                                                                                    , value = "t1"
                                                                                                                                                                                                                                                                                    }
                                                                                                                                                                                                                                                                                )
                                                                                                                                                                                                                                                                            , value = "f"
                                                                                                                                                                                                                                                                            }
                                                                                                                                                                                                                                                                        )
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
                                                                                                                                                                                                                                                            ( Expression'Column
                                                                                                                                                                                                                                                                ( Namespaced
                                                                                                                                                                                                                                                                    { namespace = Just
                                                                                                                                                                                                                                                                        ( Namespaced
                                                                                                                                                                                                                                                                            { namespace = Nothing
                                                                                                                                                                                                                                                                            , value = "t1"
                                                                                                                                                                                                                                                                            }
                                                                                                                                                                                                                                                                        )
                                                                                                                                                                                                                                                                    , value = "d"
                                                                                                                                                                                                                                                                    }
                                                                                                                                                                                                                                                                )
                                                                                                                                                                                                                                                            )
                                                                                                                                                                                                                                                        )
                                                                                                                                                                                                                                                        ( Expression'LiteralValue
                                                                                                                                                                                                                                                            ( Number "11" )
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
                                                                                                                                                                                                                                    { namespace = Just
                                                                                                                                                                                                                                        ( Namespaced
                                                                                                                                                                                                                                            { namespace = Nothing
                                                                                                                                                                                                                                            , value = "t1"
                                                                                                                                                                                                                                            }
                                                                                                                                                                                                                                        )
                                                                                                                                                                                                                                    , value = "b"
                                                                                                                                                                                                                                    }
                                                                                                                                                                                                                                )
                                                                                                                                                                                                                            ]
                                                                                                                                                                                                                        }
                                                                                                                                                                                                                    , filter = Nothing
                                                                                                                                                                                                                    , over = Nothing
                                                                                                                                                                                                                    }
                                                                                                                                                                                                                )
                                                                                                                                                                                                            ]
                                                                                                                                                                                                        }
                                                                                                                                                                                                    , filter = Nothing
                                                                                                                                                                                                    , over = Nothing
                                                                                                                                                                                                    }
                                                                                                                                                                                                )
                                                                                                                                                                                            )
                                                                                                                                                                                            ( Expression'FunctionCall
                                                                                                                                                                                                ( FunctionCallExpression
                                                                                                                                                                                                    { call = FunctionCall
                                                                                                                                                                                                        { name = Namespaced
                                                                                                                                                                                                            { namespace = Nothing
                                                                                                                                                                                                            , value = "abs"
                                                                                                                                                                                                            }
                                                                                                                                                                                                        , arguments = FunctionArguments'Arguments
                                                                                                                                                                                                            [ Expression'Column
                                                                                                                                                                                                                ( Namespaced
                                                                                                                                                                                                                    { namespace = Just
                                                                                                                                                                                                                        ( Namespaced
                                                                                                                                                                                                                            { namespace = Nothing
                                                                                                                                                                                                                            , value = "t1"
                                                                                                                                                                                                                            }
                                                                                                                                                                                                                        )
                                                                                                                                                                                                                    , value = "d"
                                                                                                                                                                                                                    }
                                                                                                                                                                                                                )
                                                                                                                                                                                                            ]
                                                                                                                                                                                                        }
                                                                                                                                                                                                    , filter = Nothing
                                                                                                                                                                                                    , over = Nothing
                                                                                                                                                                                                    }
                                                                                                                                                                                                )
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
                                                                                                                                                                                    )
                                                                                                                                                                                , Expression'Column
                                                                                                                                                                                    ( Namespaced
                                                                                                                                                                                        { namespace = Nothing
                                                                                                                                                                                        , value = "c"
                                                                                                                                                                                        }
                                                                                                                                                                                    )
                                                                                                                                                                                ) :|
                                                                                                                                                                                [
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
                                                                                                                                                                                                                ( Expression'NotEquals
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
                                                                                                                                                                                                                    ( Expression'Column
                                                                                                                                                                                                                        ( Namespaced
                                                                                                                                                                                                                            { namespace = Nothing
                                                                                                                                                                                                                            , value = "b"
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
                                                                                                                                                                                        )
                                                                                                                                                                                    , Expression'LiteralValue
                                                                                                                                                                                        ( Number "13" )
                                                                                                                                                                                    )
                                                                                                                                                                                ]
                                                                                                                                                                            , else_ = Expression'LiteralValue
                                                                                                                                                                                ( Number "11" )
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
                                                                                                                                                                    ( Expression'InValues
                                                                                                                                                                        ( InValuesExpression
                                                                                                                                                                            { expression = Expression'LiteralValue
                                                                                                                                                                                ( Number "13" )
                                                                                                                                                                            , values =
                                                                                                                                                                                [ Expression'Column
                                                                                                                                                                                    ( Namespaced
                                                                                                                                                                                        { namespace = Nothing
                                                                                                                                                                                        , value = "f"
                                                                                                                                                                                        }
                                                                                                                                                                                    )
                                                                                                                                                                                , Expression'Column
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
                                                                                                                                                                                , Expression'LiteralValue
                                                                                                                                                                                    ( Number "13" )
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
                                                                                                                                                , value = "b"
                                                                                                                                                }
                                                                                                                                            )
                                                                                                                                        ]
                                                                                                                                    }
                                                                                                                                , filter = Nothing
                                                                                                                                , over = Nothing
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
                                                                                                                                , value = "d"
                                                                                                                                }
                                                                                                                            )
                                                                                                                        )
                                                                                                                    ]
                                                                                                                }
                                                                                                            , filter = Nothing
                                                                                                            , over = Nothing
                                                                                                            }
                                                                                                        )
                                                                                                    )
                                                                                                    ( Expression'FunctionCall
                                                                                                        ( FunctionCallExpression
                                                                                                            { call = FunctionCall
                                                                                                                { name = Namespaced
                                                                                                                    { namespace = Nothing
                                                                                                                    , value = "abs"
                                                                                                                    }
                                                                                                                , arguments = FunctionArguments'Arguments
                                                                                                                    [ Expression'Column
                                                                                                                        ( Namespaced
                                                                                                                            { namespace = Just
                                                                                                                                ( Namespaced
                                                                                                                                    { namespace = Nothing
                                                                                                                                    , value = "t1"
                                                                                                                                    }
                                                                                                                                )
                                                                                                                            , value = "f"
                                                                                                                            }
                                                                                                                        )
                                                                                                                    ]
                                                                                                                }
                                                                                                            , filter = Nothing
                                                                                                            , over = Nothing
                                                                                                            }
                                                                                                        )
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
                                                                            ( Expression'NotEquals
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
                                                    ]
                                                }
                                            , filter = Nothing
                                            , over = Nothing
                                            }
                                        )
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
                                        , value = "d"
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
                            ( Expression'NotEquals
                                ( Expression'BitwiseNegate
                                    ( Expression'Column
                                        ( Namespaced
                                            { namespace = Nothing
                                            , value = "a"
                                            }
                                        )
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
                            ( Expression'NotEquals
                                ( Expression'Column
                                    ( Namespaced
                                        { namespace = Nothing
                                        , value = "d"
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