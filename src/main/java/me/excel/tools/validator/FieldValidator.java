package me.excel.tools.validator;

import me.excel.tools.model.excel.ExcelCell;

/**
 * Created by hanwen on 15-12-15.
 */
public interface FieldValidator {

  String getErrorMessage();

  String getPrompt();

  boolean validate(ExcelCell excelCell) throws SkipValidateException;

  boolean matches(ExcelCell excelCell);

  boolean matches(String field);
}
