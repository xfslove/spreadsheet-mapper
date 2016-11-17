package me.excel.tools.importer;

import me.excel.tools.ExcelSupportedDateFormat;
import me.excel.tools.factory.FileTemplate;
import me.excel.tools.factory.ModelFactory;
import me.excel.tools.model.excel.ExcelCell;
import me.excel.tools.model.excel.ExcelSheet;
import me.excel.tools.model.excel.ExcelWorkbook;
import me.excel.tools.processor.DataProcessor;
import me.excel.tools.setter.*;
import me.excel.tools.validator.cell.BooleanValidator;
import me.excel.tools.validator.cell.CellValidator;
import me.excel.tools.validator.cell.LocalDateTimeValidator;
import me.excel.tools.validator.cell.LocalDateValidator;
import org.apache.commons.lang3.StringUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * excel 文件导入器
 * <p>
 * Created by hanwen on 15-12-16.
 */
public class ExcelFileImporter implements UserFileImporter {

  protected FileTemplate fileTemplate;

  protected ExcelWorkbook excelWorkbook;

  protected ModelFactory modelFactory;

  protected List<CellValueSetter> cellValueSetters = new ArrayList<>();

  protected DefaultValueSetter defaultValueSetter = new DefaultValueSetter();

  public ExcelFileImporter(FileTemplate fileTemplate, ExcelWorkbook excelWorkbook) {
    this.fileTemplate = fileTemplate;
    this.excelWorkbook = excelWorkbook;
  }

  @Override
  public void process(DataProcessor dataProcessor) {

    if (excelWorkbook == null) {
      throw new IllegalArgumentException("excel is null");
    }
    if (dataProcessor == null) {
      throw new IllegalArgumentException("dataProcessor is null");
    }

    ExcelSheet excelSheet = excelWorkbook.getFirstSheet();

    addDefaultValueSetters();

    List models = new ArrayList<>();
    excelSheet.getDataRows().forEach(row -> {

      Object origin = modelFactory.create(row);
      Object model = modelFactory.create(row);

      dataProcessor.preProcessing(model);

      defaultValueSetter.set(model, row.getCells());

      for (ExcelCell excelCell : row.getCells()) {

        for (CellValueSetter customValueSetter : cellValueSetters) {
          if (customValueSetter.matches(excelCell)) {
            customValueSetter.set(model, excelCell);
            break;
          }
        }
      }

      dataProcessor.postProcessing(origin, model);

      models.add(model);
    });

    dataProcessor.handle(models);
  }

  @Override
  public void addCellValueSetter(CellValueSetter... setters) {
    if (setters == null) {
      return;
    }

    for (CellValueSetter setter : setters) {
      this.cellValueSetters.add(setter);
    }
  }

  @Override
  public void setModelFactory(ModelFactory modelFactory) {
    this.modelFactory = modelFactory;
  }

  private void addDefaultValueSetters() {

    Set<String> customSetters = cellValueSetters.stream().map(setter -> setter.getMatchField()).collect(Collectors.toSet());

    for (CellValidator validator : fileTemplate.getCellValidators()) {
      String matchField = validator.getMatchField();

      if (customSetters.contains(matchField)) {
        continue;
      }

      String dateTimePattern = extraPatternFromPrompt(validator.getPrompt());
      if (validator instanceof BooleanValidator) {
        addCellValueSetter(new BooleanValueSetter(matchField));
      } else if (validator instanceof LocalDateValidator) {
        addCellValueSetter(new LocalDateValueSetter(matchField, dateTimePattern));
      } else if (validator instanceof LocalDateTimeValidator) {
        addCellValueSetter(new LocalDateTimeValueSetter(matchField, dateTimePattern));
      }
    }
  }

  protected String extraPatternFromPrompt(String prompt) {

    List<String> possiblePatterns = ExcelSupportedDateFormat.getSupportedFormats().stream()
        .filter(supportedFormat -> StringUtils.indexOfIgnoreCase(prompt, supportedFormat) != -1)
        .collect(Collectors.toList());

    possiblePatterns.sort((pattern, pattern1) -> pattern1.length() - pattern.length());

    return possiblePatterns.isEmpty() ? null : possiblePatterns.get(0);
  }
}
