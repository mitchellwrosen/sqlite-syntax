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
                                                                            ( Number "17" )
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
                                                                    ( Expression'Between
                                                                        ( Expression'Column
                                                                            ( Namespaced
                                                                                { namespace = Nothing
                                                                                , value = "d"
                                                                                }
                                                                            )
                                                                        )
                                                                        ( Expression'LiteralValue
                                                                            ( Number "17" )
                                                                        )
                                                                        ( Expression'LiteralValue
                                                                            ( Number "13" )
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
                                            , Expression'Case
                                                ( CaseExpression
                                                    { base = Nothing
                                                    , cases =
                                                        ( Expression'And
                                                            ( Expression'InSubquery
                                                                ( InSubqueryExpression
                                                                    { expression = Expression'Column
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
                                                                    , subquery = SelectStatement
                                                                        { commonTableExpressions = Nothing
                                                                        , select = Union
                                                                            ( CompoundSelect
                                                                                ( SelectCore'Select
                                                                                    ( Select
                                                                                        { distinct = False
                                                                                        , columns = ResultColumn'Expression
                                                                                            ( Aliased
                                                                                                { value = Expression'Plus
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
                                                                                                            , value = "a"
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
                                                            ( Expression'GreaterThanOrEquals
                                                                ( Expression'Minus
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
                                                                ( Expression'Multiply
                                                                    ( Expression'Case
                                                                        ( CaseExpression
                                                                            { base = Nothing
                                                                            , cases =
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
                                                                                                                ( Expression'Subquery
                                                                                                                    ( SelectStatement
                                                                                                                        { commonTableExpressions = Nothing
                                                                                                                        , select = CompoundSelect
                                                                                                                            ( SelectCore'Select
                                                                                                                                ( Select
                                                                                                                                    { distinct = False
                                                                                                                                    , columns = ResultColumn'Expression
                                                                                                                                        ( Aliased
                                                                                                                                            { value = Expression'AggregateDistinctFunctionCall
                                                                                                                                                ( AggregateDistinctFunctionCallExpression
                                                                                                                                                    { call = FunctionCall
                                                                                                                                                        { name = Namespaced
                                                                                                                                                            { namespace = Nothing
                                                                                                                                                            , value = "count"
                                                                                                                                                            }
                                                                                                                                                        , arguments = Identity
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
                                                                                                                                                        }
                                                                                                                                                    , filter = Nothing
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
                                                                                ) :|
                                                                                [
                                                                                    ( Expression'LessThan
                                                                                        ( Expression'Minus
                                                                                            ( Expression'LiteralValue
                                                                                                ( Number "11" )
                                                                                            )
                                                                                            ( Expression'BitwiseNegate
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
                                                                                        )
                                                                                        ( Expression'Case
                                                                                            ( CaseExpression
                                                                                                { base = Just
                                                                                                    ( Expression'LiteralValue
                                                                                                        ( Number "13" )
                                                                                                    )
                                                                                                , cases =
                                                                                                    ( Expression'Column
                                                                                                        ( Namespaced
                                                                                                            { namespace = Nothing
                                                                                                            , value = "f"
                                                                                                            }
                                                                                                        )
                                                                                                    , Expression'LiteralValue
                                                                                                        ( Number "19" )
                                                                                                    ) :| []
                                                                                                , else_ = Expression'FunctionCall
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
                                                                                                                                            { value = Expression'Negate
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
                                                                                            , value = "e"
                                                                                            }
                                                                                        )
                                                                                    )
                                                                                ]
                                                                            , else_ = Expression'LiteralValue
                                                                                ( Number "13" )
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
                                                        ) :| []
                                                    , else_ = Expression'Column
                                                        ( Namespaced
                                                            { namespace = Nothing
                                                            , value = "b"
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
                            ( Expression'GreaterThanOrEquals
                                ( Expression'Negate
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
                                                                                                    ( Number "11" )
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
                                                                                ( Expression'Or
                                                                                    ( Expression'InValues
                                                                                        ( InValuesExpression
                                                                                            { expression = Expression'Divide
                                                                                                ( Expression'FunctionCall
                                                                                                    ( FunctionCallExpression
                                                                                                        { call = FunctionCall
                                                                                                            { name = Namespaced
                                                                                                                { namespace = Nothing
                                                                                                                , value = "abs"
                                                                                                                }
                                                                                                            , arguments = FunctionArguments'Arguments
                                                                                                                [ Expression'Negate
                                                                                                                    ( Expression'Case
                                                                                                                        ( CaseExpression
                                                                                                                            { base = Just
                                                                                                                                ( Expression'LiteralValue
                                                                                                                                    ( Number "19" )
                                                                                                                                )
                                                                                                                            , cases =
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
                                                                                                                                                                                                                                                            { value = Expression'LiteralValue
                                                                                                                                                                                                                                                                ( Number "13" )
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
                                                                                                                                                                                                                , value = "f"
                                                                                                                                                                                                                }
                                                                                                                                                                                                            )
                                                                                                                                                                                                        , values =
                                                                                                                                                                                                            [ Expression'LiteralValue
                                                                                                                                                                                                                ( Number "13" )
                                                                                                                                                                                                            , Expression'Column
                                                                                                                                                                                                                ( Namespaced
                                                                                                                                                                                                                    { namespace = Nothing
                                                                                                                                                                                                                    , value = "e"
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
                                                                                                                                                                                                            ]
                                                                                                                                                                                                        }
                                                                                                                                                                                                    )
                                                                                                                                                                                                )
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
                                                                                                                                                                                                    ( Expression'LiteralValue
                                                                                                                                                                                                        ( Number "13" )
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
                                                                                                                                                    [ Expression'LiteralValue
                                                                                                                                                        ( Number "13" )
                                                                                                                                                    ]
                                                                                                                                                }
                                                                                                                                            , filter = Nothing
                                                                                                                                            , over = Nothing
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
                                                                                                                                        , value = "b"
                                                                                                                                        }
                                                                                                                                    )
                                                                                                                                ) :| []
                                                                                                                            , else_ = Expression'Column
                                                                                                                                ( Namespaced
                                                                                                                                    { namespace = Nothing
                                                                                                                                    , value = "d"
                                                                                                                                    }
                                                                                                                                )
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
                                                                                                                [ Expression'LiteralValue
                                                                                                                    ( Number "13" )
                                                                                                                ]
                                                                                                            }
                                                                                                        , filter = Nothing
                                                                                                        , over = Nothing
                                                                                                        }
                                                                                                    )
                                                                                                )
                                                                                            , values =
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
                                                                                        )
                                                                                    )
                                                                                    ( Expression'Between
                                                                                        ( Expression'LiteralValue
                                                                                            ( Number "17" )
                                                                                        )
                                                                                        ( Expression'LiteralValue
                                                                                            ( Number "13" )
                                                                                        )
                                                                                        ( Expression'Column
                                                                                            ( Namespaced
                                                                                                { namespace = Nothing
                                                                                                , value = "f"
                                                                                                }
                                                                                            )
                                                                                        )
                                                                                    )
                                                                                )
                                                                                ( Expression'And
                                                                                    ( Expression'GreaterThan
                                                                                        ( Expression'Column
                                                                                            ( Namespaced
                                                                                                { namespace = Nothing
                                                                                                , value = "f"
                                                                                                }
                                                                                            )
                                                                                        )
                                                                                        ( Expression'LiteralValue
                                                                                            ( Number "11" )
                                                                                        )
                                                                                    )
                                                                                    ( Expression'LessThan
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
                                                    , Expression'Case
                                                        ( CaseExpression
                                                            { base = Nothing
                                                            , cases =
                                                                ( Expression'Not
                                                                    ( Expression'Between
                                                                        ( Expression'LiteralValue
                                                                            ( Number "19" )
                                                                        )
                                                                        ( Expression'Negate
                                                                            ( Expression'LiteralValue
                                                                                ( Number "17" )
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
                                                                , Expression'LiteralValue
                                                                    ( Number "11" )
                                                                ) :|
                                                                [
                                                                    ( Expression'Equals
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
                                                                                , value = "d"
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
                                                                            , value = "a"
                                                                            }
                                                                        )
                                                                    )
                                                                ]
                                                            , else_ = Expression'Column
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