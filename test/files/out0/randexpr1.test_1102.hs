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
                                                                                        [ Expression'BitwiseNegate
                                                                                            ( Expression'LiteralValue
                                                                                                ( Number "19" )
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
                                                                    ( Expression'InSubquery
                                                                        ( InSubqueryExpression
                                                                            { expression = Expression'LiteralValue
                                                                                ( Number "11" )
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
                                                                                                            ( Expression'Minus
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
                                                                                                                ( Expression'Multiply
                                                                                                                    ( Expression'LiteralValue
                                                                                                                        ( Number "11" )
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
                                                                                                                                                                ( Expression'BitwiseOr
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
                                                                                                                                                                                                                                        , value = "e"
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
                                                                                                                                                                                                            ( Expression'Between
                                                                                                                                                                                                                ( Expression'Plus
                                                                                                                                                                                                                    ( Expression'Multiply
                                                                                                                                                                                                                        ( Expression'Column
                                                                                                                                                                                                                            ( Namespaced
                                                                                                                                                                                                                                { namespace = Nothing
                                                                                                                                                                                                                                , value = "c"
                                                                                                                                                                                                                                }
                                                                                                                                                                                                                            )
                                                                                                                                                                                                                        )
                                                                                                                                                                                                                        ( Expression'Case
                                                                                                                                                                                                                            ( CaseExpression
                                                                                                                                                                                                                                { base = Just
                                                                                                                                                                                                                                    ( Expression'Plus
                                                                                                                                                                                                                                        ( Expression'Multiply
                                                                                                                                                                                                                                            ( Expression'LiteralValue
                                                                                                                                                                                                                                                ( Number "19" )
                                                                                                                                                                                                                                            )
                                                                                                                                                                                                                                            ( Expression'LiteralValue
                                                                                                                                                                                                                                                ( Number "19" )
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
                                                                                                                                                                                                                                                                                    ( Expression'Not
                                                                                                                                                                                                                                                                                        ( Expression'InValues
                                                                                                                                                                                                                                                                                            ( InValuesExpression
                                                                                                                                                                                                                                                                                                { expression = Expression'LiteralValue
                                                                                                                                                                                                                                                                                                    ( Number "11" )
                                                                                                                                                                                                                                                                                                , values =
                                                                                                                                                                                                                                                                                                    [ Expression'Plus
                                                                                                                                                                                                                                                                                                        ( Expression'LiteralValue
                                                                                                                                                                                                                                                                                                            ( Number "13" )
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
                                                                                                                                                                                                                                                                                                    , Expression'Column
                                                                                                                                                                                                                                                                                                        ( Namespaced
                                                                                                                                                                                                                                                                                                            { namespace = Nothing
                                                                                                                                                                                                                                                                                                            , value = "b"
                                                                                                                                                                                                                                                                                                            }
                                                                                                                                                                                                                                                                                                        )
                                                                                                                                                                                                                                                                                                    , Expression'LiteralValue
                                                                                                                                                                                                                                                                                                        ( Number "11" )
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
                                                                                                                                                                                                                                                    , value = "d"
                                                                                                                                                                                                                                                    }
                                                                                                                                                                                                                                                )
                                                                                                                                                                                                                                            )
                                                                                                                                                                                                                                        )
                                                                                                                                                                                                                                    )
                                                                                                                                                                                                                                , cases =
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
                                                                                                                                                                                                                                        { namespace = Just
                                                                                                                                                                                                                                            ( Namespaced
                                                                                                                                                                                                                                                { namespace = Nothing
                                                                                                                                                                                                                                                , value = "t1"
                                                                                                                                                                                                                                                }
                                                                                                                                                                                                                                            )
                                                                                                                                                                                                                                        , value = "b"
                                                                                                                                                                                                                                        }
                                                                                                                                                                                                                                    )
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
                                                                                                                                                                    )
                                                                                                                                                                    ( Expression'Column
                                                                                                                                                                        ( Namespaced
                                                                                                                                                                            { namespace = Nothing
                                                                                                                                                                            , value = "f"
                                                                                                                                                                            }
                                                                                                                                                                        )
                                                                                                                                                                    )
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
                                                                                                                                    , Expression'Negate
                                                                                                                                        ( Expression'Negate
                                                                                                                                            ( Expression'LiteralValue
                                                                                                                                                ( Number "17" )
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
                                                                                                            ( Expression'Column
                                                                                                                ( Namespaced
                                                                                                                    { namespace = Nothing
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
                                                                                                    { value = Expression'Negate
                                                                                                        ( Expression'LiteralValue
                                                                                                            ( Number "19" )
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
                            ( Expression'Between
                                ( Expression'BitwiseNegate
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
                                ( Expression'Minus
                                    ( Expression'Minus
                                        ( Expression'Minus
                                            ( Expression'Multiply
                                                ( Expression'LiteralValue
                                                    ( Number "11" )
                                                )
                                                ( Expression'LiteralValue
                                                    ( Number "13" )
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
                                    ( Expression'Column
                                        ( Namespaced
                                            { namespace = Nothing
                                            , value = "f"
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