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
                                                                        { value = Expression'LiteralValue
                                                                            ( Number "19" )
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
                                                                        ( Expression'Minus
                                                                            ( Expression'Case
                                                                                ( CaseExpression
                                                                                    { base = Just
                                                                                        ( Expression'Divide
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
                                                                                                                    { namespace = Nothing
                                                                                                                    , value = "e"
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
                                                                                                            [ Expression'Case
                                                                                                                ( CaseExpression
                                                                                                                    { base = Just
                                                                                                                        ( Expression'Plus
                                                                                                                            ( Expression'Column
                                                                                                                                ( Namespaced
                                                                                                                                    { namespace = Nothing
                                                                                                                                    , value = "b"
                                                                                                                                    }
                                                                                                                                )
                                                                                                                            )
                                                                                                                            ( Expression'Column
                                                                                                                                ( Namespaced
                                                                                                                                    { namespace = Nothing
                                                                                                                                    , value = "f"
                                                                                                                                    }
                                                                                                                                )
                                                                                                                            )
                                                                                                                        )
                                                                                                                    , cases =
                                                                                                                        ( Expression'Column
                                                                                                                            ( Namespaced
                                                                                                                                { namespace = Nothing
                                                                                                                                , value = "b"
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
                                                                                                                        ) :| []
                                                                                                                    , else_ = Expression'Column
                                                                                                                        ( Namespaced
                                                                                                                            { namespace = Nothing
                                                                                                                            , value = "e"
                                                                                                                            }
                                                                                                                        )
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
                                                                                    , cases =
                                                                                        ( Expression'Minus
                                                                                            ( Expression'Plus
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
                                                                                                                                        ( Expression'And
                                                                                                                                            ( Expression'And
                                                                                                                                                ( Expression'LessThanOrEquals
                                                                                                                                                    ( Expression'LiteralValue
                                                                                                                                                        ( Number "19" )
                                                                                                                                                    )
                                                                                                                                                    ( Expression'Column
                                                                                                                                                        ( Namespaced
                                                                                                                                                            { namespace = Nothing
                                                                                                                                                            , value = "e"
                                                                                                                                                            }
                                                                                                                                                        )
                                                                                                                                                    )
                                                                                                                                                )
                                                                                                                                                ( Expression'GreaterThanOrEquals
                                                                                                                                                    ( Expression'Multiply
                                                                                                                                                        ( Expression'Column
                                                                                                                                                            ( Namespaced
                                                                                                                                                                { namespace = Nothing
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
                                                                                                                                                    ( Expression'Negate
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
                                                                                                                                            )
                                                                                                                                            ( Expression'GreaterThanOrEquals
                                                                                                                                                ( Expression'Column
                                                                                                                                                    ( Namespaced
                                                                                                                                                        { namespace = Nothing
                                                                                                                                                        , value = "c"
                                                                                                                                                        }
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
                                                                                                                , Expression'Plus
                                                                                                                    ( Expression'Plus
                                                                                                                        ( Expression'LiteralValue
                                                                                                                            ( Number "11" )
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
                                                                                                                            , value = "a"
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
                                                                                        , Expression'LiteralValue
                                                                                            ( Number "17" )
                                                                                        ) :| []
                                                                                    , else_ = Expression'LiteralValue
                                                                                        ( Number "17" )
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
                                            , Expression'Column
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
                        ( Expression'Not
                            ( Expression'NotEquals
                                ( Expression'Plus
                                    ( Expression'Plus
                                        ( Expression'Multiply
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
                                                    , value = "f"
                                                    }
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
                                    ( Expression'Multiply
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
                                        ( Expression'LiteralValue
                                            ( Number "13" )
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