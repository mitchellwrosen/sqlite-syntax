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
                                                                        { value = Expression'Column
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
                                                                                            ( Expression'GreaterThan
                                                                                                ( Expression'Divide
                                                                                                    ( Expression'FunctionCall
                                                                                                        ( FunctionCallExpression
                                                                                                            { call = FunctionCall
                                                                                                                { name = Namespaced
                                                                                                                    { namespace = Nothing
                                                                                                                    , value = "abs"
                                                                                                                    }
                                                                                                                , arguments = FunctionArguments'Arguments
                                                                                                                    [ Expression'Multiply
                                                                                                                        ( Expression'BitwiseNegate
                                                                                                                            ( Expression'Case
                                                                                                                                ( CaseExpression
                                                                                                                                    { base = Just
                                                                                                                                        ( Expression'Minus
                                                                                                                                            ( Expression'Minus
                                                                                                                                                ( Expression'Minus
                                                                                                                                                    ( Expression'Multiply
                                                                                                                                                        ( Expression'BitwiseNegate
                                                                                                                                                            ( Expression'LiteralValue
                                                                                                                                                                ( Number "13" )
                                                                                                                                                            )
                                                                                                                                                        )
                                                                                                                                                        ( Expression'Case
                                                                                                                                                            ( CaseExpression
                                                                                                                                                                { base = Nothing
                                                                                                                                                                , cases =
                                                                                                                                                                    ( Expression'InSubquery
                                                                                                                                                                        ( InSubqueryExpression
                                                                                                                                                                            { expression = Expression'Column
                                                                                                                                                                                ( Namespaced
                                                                                                                                                                                    { namespace = Nothing
                                                                                                                                                                                    , value = "c"
                                                                                                                                                                                    }
                                                                                                                                                                                )
                                                                                                                                                                            , subquery = SelectStatement
                                                                                                                                                                                { commonTableExpressions = Nothing
                                                                                                                                                                                , select = Union
                                                                                                                                                                                    ( CompoundSelect
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
                                                                                                                                                                                                                        [ Expression'Multiply
                                                                                                                                                                                                                            ( Expression'Case
                                                                                                                                                                                                                                ( CaseExpression
                                                                                                                                                                                                                                    { base = Nothing
                                                                                                                                                                                                                                    , cases =
                                                                                                                                                                                                                                        ( Expression'NotEquals
                                                                                                                                                                                                                                            ( Expression'Column
                                                                                                                                                                                                                                                ( Namespaced
                                                                                                                                                                                                                                                    { namespace = Nothing
                                                                                                                                                                                                                                                    , value = "b"
                                                                                                                                                                                                                                                    }
                                                                                                                                                                                                                                                )
                                                                                                                                                                                                                                            )
                                                                                                                                                                                                                                            ( Expression'LiteralValue
                                                                                                                                                                                                                                                ( Number "11" )
                                                                                                                                                                                                                                            )
                                                                                                                                                                                                                                        , Expression'Column
                                                                                                                                                                                                                                            ( Namespaced
                                                                                                                                                                                                                                                { namespace = Nothing
                                                                                                                                                                                                                                                , value = "d"
                                                                                                                                                                                                                                                }
                                                                                                                                                                                                                                            )
                                                                                                                                                                                                                                        ) :|
                                                                                                                                                                                                                                        [
                                                                                                                                                                                                                                            ( Expression'Not
                                                                                                                                                                                                                                                ( Expression'InValues
                                                                                                                                                                                                                                                    ( InValuesExpression
                                                                                                                                                                                                                                                        { expression = Expression'LiteralValue
                                                                                                                                                                                                                                                            ( Number "13" )
                                                                                                                                                                                                                                                        , values =
                                                                                                                                                                                                                                                            [ Expression'LiteralValue
                                                                                                                                                                                                                                                                ( Number "19" )
                                                                                                                                                                                                                                                            , Expression'Column
                                                                                                                                                                                                                                                                ( Namespaced
                                                                                                                                                                                                                                                                    { namespace = Nothing
                                                                                                                                                                                                                                                                    , value = "c"
                                                                                                                                                                                                                                                                    }
                                                                                                                                                                                                                                                                )
                                                                                                                                                                                                                                                            , Expression'Column
                                                                                                                                                                                                                                                                ( Namespaced
                                                                                                                                                                                                                                                                    { namespace = Nothing
                                                                                                                                                                                                                                                                    , value = "e"
                                                                                                                                                                                                                                                                    }
                                                                                                                                                                                                                                                                )
                                                                                                                                                                                                                                                            ]
                                                                                                                                                                                                                                                        }
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
                                                                                                                                                                                                                                                    , value = "f"
                                                                                                                                                                                                                                                    }
                                                                                                                                                                                                                                                )
                                                                                                                                                                                                                                            )
                                                                                                                                                                                                                                        ]
                                                                                                                                                                                                                                    , else_ = Expression'LiteralValue
                                                                                                                                                                                                                                        ( Number "19" )
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
                                                                                                                                                                                    )
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
                                                                                                                                                                                                                        , value = "abs"
                                                                                                                                                                                                                        }
                                                                                                                                                                                                                    , arguments = FunctionArguments'Arguments
                                                                                                                                                                                                                        [ Expression'FunctionCall
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
                                                                                                                                                                                                                        , value = "min"
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
                                                                                                                                                                    ) :| []
                                                                                                                                                                , else_ = Expression'Column
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
                                                                                                                                                ( Expression'LiteralValue
                                                                                                                                                    ( Number "13" )
                                                                                                                                                )
                                                                                                                                            )
                                                                                                                                            ( Expression'Multiply
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
                                                                                                                                                ( Expression'LiteralValue
                                                                                                                                                    ( Number "17" )
                                                                                                                                                )
                                                                                                                                            )
                                                                                                                                        )
                                                                                                                                    , cases =
                                                                                                                                        ( Expression'Column
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
                                                                                                                                                , value = "a"
                                                                                                                                                }
                                                                                                                                            )
                                                                                                                                        ) :| []
                                                                                                                                    , else_ = Expression'LiteralValue
                                                                                                                                        ( Number "13" )
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
                                                                                                                                , value = "c"
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
                                            , Expression'LiteralValue
                                                ( Number "13" )
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
                            ( Expression'GreaterThan
                                ( Expression'Case
                                    ( CaseExpression
                                        { base = Nothing
                                        , cases =
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
                                                        , value = "a"
                                                        }
                                                    )
                                                )
                                            , Expression'LiteralValue
                                                ( Number "17" )
                                            ) :| []
                                        , else_ = Expression'LiteralValue
                                            ( Number "13" )
                                        }
                                    )
                                )
                                ( Expression'Plus
                                    ( Expression'Column
                                        ( Namespaced
                                            { namespace = Nothing
                                            , value = "f"
                                            }
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
                                                                                ( Expression'Not
                                                                                    ( Expression'InValues
                                                                                        ( InValuesExpression
                                                                                            { expression = Expression'LiteralValue
                                                                                                ( Number "11" )
                                                                                            , values =
                                                                                                [ Expression'Plus
                                                                                                    ( Expression'Case
                                                                                                        ( CaseExpression
                                                                                                            { base = Nothing
                                                                                                            , cases =
                                                                                                                ( Expression'And
                                                                                                                    ( Expression'LessThan
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
                                                                                                                        ( Expression'Divide
                                                                                                                            ( Expression'FunctionCall
                                                                                                                                ( FunctionCallExpression
                                                                                                                                    { call = FunctionCall
                                                                                                                                        { name = Namespaced
                                                                                                                                            { namespace = Nothing
                                                                                                                                            , value = "abs"
                                                                                                                                            }
                                                                                                                                        , arguments = FunctionArguments'Arguments
                                                                                                                                            [ Expression'LiteralValue
                                                                                                                                                ( Number "11" )
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
                                                                                                                                            [ Expression'Multiply
                                                                                                                                                ( Expression'Multiply
                                                                                                                                                    ( Expression'LiteralValue
                                                                                                                                                        ( Number "19" )
                                                                                                                                                    )
                                                                                                                                                    ( Expression'Column
                                                                                                                                                        ( Namespaced
                                                                                                                                                            { namespace = Nothing
                                                                                                                                                            , value = "d"
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
                                                                                                                                                        , value = "c"
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
                                                                                                                    )
                                                                                                                    ( Expression'Or
                                                                                                                        ( Expression'InSubquery
                                                                                                                            ( InSubqueryExpression
                                                                                                                                { expression = Expression'Column
                                                                                                                                    ( Namespaced
                                                                                                                                        { namespace = Nothing
                                                                                                                                        , value = "c"
                                                                                                                                        }
                                                                                                                                    )
                                                                                                                                , subquery = SelectStatement
                                                                                                                                    { commonTableExpressions = Nothing
                                                                                                                                    , select = Union
                                                                                                                                        ( CompoundSelect
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
                                                                                                                                                                    , value = "c"
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
                                                                                                                                        )
                                                                                                                                        ( SelectCore'Select
                                                                                                                                            ( Select
                                                                                                                                                { distinct = False
                                                                                                                                                , columns = ResultColumn'Expression
                                                                                                                                                    ( Aliased
                                                                                                                                                        { value = Expression'Column
                                                                                                                                                            ( Namespaced
                                                                                                                                                                { namespace = Nothing
                                                                                                                                                                , value = "f"
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
                                                                                                                                }
                                                                                                                            )
                                                                                                                        )
                                                                                                                        ( Expression'GreaterThan
                                                                                                                            ( Expression'Column
                                                                                                                                ( Namespaced
                                                                                                                                    { namespace = Nothing
                                                                                                                                    , value = "a"
                                                                                                                                    }
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
                                                                                                                ) :|
                                                                                                                [
                                                                                                                    ( Expression'Or
                                                                                                                        ( Expression'Not
                                                                                                                            ( Expression'Between
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
                                                                                                                                        , value = "a"
                                                                                                                                        }
                                                                                                                                    )
                                                                                                                                )
                                                                                                                            )
                                                                                                                        )
                                                                                                                        ( Expression'Not
                                                                                                                            ( Expression'InValues
                                                                                                                                ( InValuesExpression
                                                                                                                                    { expression = Expression'Column
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
                                                                                                                                    , values =
                                                                                                                                        [ Expression'Negate
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
                                                                                                                                        , Expression'LiteralValue
                                                                                                                                            ( Number "17" )
                                                                                                                                        , Expression'Column
                                                                                                                                            ( Namespaced
                                                                                                                                                { namespace = Nothing
                                                                                                                                                , value = "f"
                                                                                                                                                }
                                                                                                                                            )
                                                                                                                                        ]
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
                                                                                                                            , value = "d"
                                                                                                                            }
                                                                                                                        )
                                                                                                                    )
                                                                                                                ]
                                                                                                            , else_ = Expression'Column
                                                                                                                ( Namespaced
                                                                                                                    { namespace = Nothing
                                                                                                                    , value = "e"
                                                                                                                    }
                                                                                                                )
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
                                                                                                , Expression'LiteralValue
                                                                                                    ( Number "19" )
                                                                                                , Expression'LiteralValue
                                                                                                    ( Number "17" )
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
                                                        , Expression'LiteralValue
                                                            ( Number "17" )
                                                        ]
                                                    }
                                                , filter = Nothing
                                                , over = Nothing
                                                }
                                            )
                                        )
                                        ( Expression'Negate
                                            ( Expression'LiteralValue
                                                ( Number "11" )
                                            )
                                        )
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