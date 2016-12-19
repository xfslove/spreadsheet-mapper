package me.excel.tools.validator;

import me.excel.tools.validator.cell.CellValidator;
import me.excel.tools.validator.row.RowValidator;
import me.excel.tools.validator.workbook.WorkbookValidator;

/**
 * file validator
 * <p>
 * Created by hanwen on 15-12-16.
 */
public interface UserFileValidator {

  /**
   * 设置支持的field范围
   *
   * @param fields
   */
  void setFieldScope(String... fields);

  /**
   * 设置必须的field范围
   *
   * @param fields
   */
  void setRequiredFields(String... fields);

  /**
   * 增加单元格验证器
   *
   * @param validators
   */
  void addCellValidator(CellValidator... validators);

  /**
   * 增加行验证器行验证器, 在单元格验证器之后验证
   *
   * @param validators
   */
  void addRowValidator(RowValidator... validators);

  /**
   * 增加工作表验证器
   *
   * @param validators
   */
  void addWorkbookValidator(WorkbookValidator... validators);

  boolean validate();

  void writeFailureMessageComments();
}
