package me.excel.tools.validator.template.sheet;

import me.excel.tools.validator.template.TemplateValidator;

/**
 * excel sheet template validator, after workbook template validators, if workbook template validators failure, sheet template validators will skip.
 * <p>
 * Created by hanwen on 2016/12/23.
 */
public interface SheetTemplateValidator extends TemplateValidator {

  /**
   * validate supplied excel sheet with template
   *
   * @param template sheet
   * @return result
   */
  boolean validate(ExcelSheetTemplate template);
}
