package me.excel.tools.factory;

import me.excel.tools.importer.UserFileImporter;
import me.excel.tools.validator.UserFileValidator;
import me.excel.tools.validator.cell.CellValidator;
import me.excel.tools.validator.row.RowValidator;
import me.excel.tools.validator.workbook.WorkbookValidator;

import java.io.IOException;
import java.util.List;
import java.util.Set;

/**
 * template factory
 * <p>
 * Created by hanwen on 15-12-16.
 */
public interface FileTemplate {

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
   * 增加行验证器
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

  /**
   * 得到file中field字段的所有值
   *
   * @param field
   * @return
   * @throws IOException
   */
  Set<String> getCellValuesOfField(String field);

  List<String> getFieldScope();

  List<String> getRequiredFields();

  List<CellValidator> getCellValidators();

  List<WorkbookValidator> getWorkbookValidators();

  /**
   * 行验证器在单元格验证器之后
   */
  List<RowValidator> getRowValidators();

  /**
   * 获得模板对应的file factory
   *
   * @return
   */
  UserFileFactory getUserFileFactory();

  UserFileValidator getUserFileValidator();

  /**
   * 获得模板对应的file importer
   *
   * @return
   */
  UserFileImporter getUserFileImporter();
}
