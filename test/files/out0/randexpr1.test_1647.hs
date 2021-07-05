Statement'Select
    ( SelectStatement
        { commonTableExpressions = Nothing
        , select = CompoundSelect
            ( SelectCore'Select
                ( Select
                    { distinct = False
                    , columns = ResultColumn'Expression
                        ( Aliased
                            { value = Expression'BitwiseAnd
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
                                                                                                                [ Expression'Minus
                                                                                                                    ( Expression'Multiply
                                                                                                                        ( Expression'Multiply
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
                                                                                                                        ( Expression'Case
                                                                                                                            ( CaseExpression
                                                                                                                                { base = Nothing
                                                                                                                                , cases =
                                                                                                                                    ( Expression'LessThan
                                                                                                                                        ( Expression'Multiply
                                                                                                                                            ( Expression'Column
                                                                                                                                                ( Namespaced
                                                                                                                                                    { namespace = Nothing
                                                                                                                                                    , value = "f"
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
                                                                                                                                        )
                                                                                                                                        ( Expression'Plus
                                                                                                                                            ( Expression'Plus
                                                                                                                                                ( Expression'Plus
                                                                                                                                                    ( Expression'Multiply
                                                                                                                                                        ( Expression'Multiply
                                                                                                                                                            ( Expression'Column
                                                                                                                                                                ( Namespaced
                                                                                                                                                                    { namespace = Nothing
                                                                                                                                                                    , value = "a"
                                                                                                                                                                    }
                                                                                                                                                                )
                                                                                                                                                            )
                                                                                                                                                            ( Expression'LiteralValue
                                                                                                                                                                ( Number "17" )
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
                                                                                                                                                                    , value = "c"
                                                                                                                                                                    }
                                                                                                                                                                )
                                                                                                                                                            )
                                                                                                                                                        )
                                                                                                                                                    )
                                                                                                                                                    ( Expression'Multiply
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
                                                                                                                                                                                                ( Expression'GreaterThan
                                                                                                                                                                                                    ( Expression'LiteralValue
                                                                                                                                                                                                        ( Number "11" )
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
                                                                                                                                                                        , Expression'Multiply
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
                                                                                                                                                                            ( Expression'LiteralValue
                                                                                                                                                                                ( Number "19" )
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
                                                                                                                                                                { namespace = Nothing
                                                                                                                                                                , value = "e"
                                                                                                                                                                }
                                                                                                                                                            )
                                                                                                                                                        )
                                                                                                                                                    )
                                                                                                                                                )
                                                                                                                                                ( Expression'Column
                                                                                                                                                    ( Namespaced
                                                                                                                                                        { namespace = Nothing
                                                                                                                                                        , value = "c"
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
                                                                                                                                    ) :|
                                                                                                                                    [
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
                                                                                                                                                                ( Expression'GreaterThanOrEquals
                                                                                                                                                                    ( Expression'LiteralValue
                                                                                                                                                                        ( Number "13" )
                                                                                                                                                                    )
                                                                                                                                                                    ( Expression'Column
                                                                                                                                                                        ( Namespaced
                                                                                                                                                                            { namespace = Nothing
                                                                                                                                                                            , value = "e"
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
                                                                                                                                                , value = "e"
                                                                                                                                                }
                                                                                                                                            )
                                                                                                                                        )
                                                                                                                                    ]
                                                                                                                                , else_ = Expression'Column
                                                                                                                                    ( Namespaced
                                                                                                                                        { namespace = Nothing
                                                                                                                                        , value = "f"
                                                                                                                                        }
                                                                                                                                    )
                                                                                                                                }
                                                                                                                            )
                                                                                                                        )
                                                                                                                    )
                                                                                                                    ( Expression'Multiply
                                                                                                                        ( Expression'Column
                                                                                                                            ( Namespaced
                                                                                                                                { namespace = Nothing
                                                                                                                                , value = "f"
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
                                                                                                                [ Expression'LiteralValue
                                                                                                                    ( Number "13" )
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
                                                                        ( Expression'LessThanOrEquals
                                                                            ( Expression'Column
                                                                                ( Namespaced
                                                                                    { namespace = Nothing
                                                                                    , value = "a"
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
                                                , Expression'LiteralValue
                                                    ( Number "13" )
                                                ]
                                            }
                                        , filter = Nothing
                                        , over = Nothing
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
                                        , value = "c"
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