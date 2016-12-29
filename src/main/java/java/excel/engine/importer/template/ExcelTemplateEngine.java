package java.excel.engine.importer.template;

import java.excel.engine.importer.processor.ObjectProcessorEngine;
import java.excel.engine.importer.validator.ExcelValidatorEngine;
import java.excel.engine.model.ext.SheetTemplate;
import java.excel.engine.model.ext.SheetTemplateBean;
import java.util.Set;

/**
 * excel import template engine
 * <p>
 * Created by hanwen on 15-12-16.
 */
public interface ExcelTemplateEngine {

  /**
   * <pre>
   * one sheet one template (one to one),
   * if you pass templates with same sheet index ({@link SheetTemplate#getSheetIndex()}),
   * after add will override before add
   * if you pass templates null means using default template {@link SheetTemplateBean#DEFAULT(int)}
   * </pre>
   *
   * @param sheetTemplates
   */
  void addSheetTemplate(SheetTemplate... sheetTemplates);

  /**
   * @return excelValidatorEngine
   * @see ExcelValidatorEngine
   */
  ExcelValidatorEngine getExcelValidatorEngine();

  /**
   * @return objectProcessorEngine
   * @see ObjectProcessorEngine
   */
  ObjectProcessorEngine getObjectProcessorEngine();

  /**
   * @param sheetIndex which sheet
   * @param field      field
   * @return distinct values of supplied field
   */
  Set<String> getDistinctValuesOfField(int sheetIndex, String field);
}
