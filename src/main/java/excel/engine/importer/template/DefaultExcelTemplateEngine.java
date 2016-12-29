package excel.engine.importer.template;

import excel.engine.importer.processor.ObjectProcessorEngine;
import excel.engine.model.ext.SheetTemplateBean;
import excel.engine.exception.ExcelReadException;
import excel.engine.importer.processor.DefaultObjectProcessorEngine;
import excel.engine.importer.validator.DefaultExcelValidatorEngine;
import excel.engine.importer.validator.ExcelValidatorEngine;
import excel.engine.model.excel.Sheet;
import excel.engine.model.excel.Workbook;
import excel.engine.model.ext.SheetTemplate;
import excel.engine.util.ExcelReadHelper;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Set;

/**
 * Created by hanwen on 15-12-16.
 */
public class DefaultExcelTemplateEngine implements ExcelTemplateEngine {

  private Workbook workbook;

  private ExcelValidatorEngine excelValidatorEngine;

  private ObjectProcessorEngine objectProcessorEngine;

  public DefaultExcelTemplateEngine(File excel, SheetTemplate... sheetTemplates) throws IOException {
    this(new FileInputStream(excel), sheetTemplates);
  }

  /**
   * <pre>
   * one sheet one template (one to one),
   * if you pass templates with same sheet index ({@link SheetTemplate#getSheetIndex()}),
   * after add will override before add
   * if you pass templates null means using default template {@link SheetTemplateBean#DEFAULT(int)}
   * </pre>
   *
   * @param inputStream    read file stream
   * @param sheetTemplates sheet templates
   * @throws IOException io exception
   */
  public DefaultExcelTemplateEngine(InputStream inputStream, SheetTemplate... sheetTemplates) throws IOException {

    workbook = ExcelReadHelper.read(inputStream, sheetTemplates);

    objectProcessorEngine = new DefaultObjectProcessorEngine(workbook);

    excelValidatorEngine = new DefaultExcelValidatorEngine(workbook);
  }

  @Override
  public ExcelValidatorEngine getExcelValidatorEngine() {
    return excelValidatorEngine;
  }

  @Override
  public ObjectProcessorEngine getObjectProcessorEngine() {
    return objectProcessorEngine;
  }

  @Override
  public Set<String> getDistinctValuesOfField(int sheetIndex, String field) {

    if (workbook == null) {
      throw new ExcelReadException("workbook is null");
    }

    Sheet sheet = workbook.getSheet(sheetIndex);

    if (sheet == null) {
      throw new ExcelReadException("sheet index out of bounds");
    }

    return sheet.getDistinctValuesOfField(field);
  }

}