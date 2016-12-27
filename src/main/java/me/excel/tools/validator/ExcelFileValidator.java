package me.excel.tools.validator;

import me.excel.tools.exception.ExcelReadException;
import me.excel.tools.model.excel.ExcelRow;
import me.excel.tools.model.excel.ExcelSheet;
import me.excel.tools.model.excel.ExcelWorkbook;
import me.excel.tools.model.message.DataValidateMessage;
import me.excel.tools.model.message.TemplateValidateMessage;
import me.excel.tools.model.message.ValidateResult;
import me.excel.tools.validator.data.DataValidator;
import me.excel.tools.validator.data.cell.CellValidator;
import me.excel.tools.validator.data.row.RowValidator;
import me.excel.tools.validator.data.sheet.SheetValidator;
import me.excel.tools.validator.data.workbook.WorkbookValidator;
import me.excel.tools.validator.template.sheet.SheetTemplateValidator;
import me.excel.tools.validator.template.workbook.WorkbookTemplateValidator;
import org.apache.commons.collections.CollectionUtils;

import java.util.*;

/**
 * Created by hanwen on 15-12-16.
 */
public class ExcelFileValidator implements UserFileValidator {

  /*===================
    template validators
   ====================*/
  private List<WorkbookTemplateValidator> workbookTemplateValidators = new ArrayList<>();
  private List<SheetTemplateValidator> sheetTemplateValidators = new ArrayList<>();

  /*===============
    data validators
   ================*/
  private List<WorkbookValidator> workbookValidators = new ArrayList<>();
  private List<SheetValidator> sheetValidators = new ArrayList<>();
  private List<RowValidator> rowValidators = new ArrayList<>();
  private List<CellValidator> cellValidators = new ArrayList<>();

  /*==============
    error messages
   ===============*/
  private List<TemplateValidateMessage> templateErrorMessages = new ArrayList<>();
  private List<DataValidateMessage> dataErrorMessages = new ArrayList<>();

  private ExcelWorkbook excelWorkbook;

  public ExcelFileValidator(ExcelWorkbook excelWorkbook) {
    this.excelWorkbook = excelWorkbook;
  }

  @Override
  public void addWorkbookTemplateValidator(WorkbookTemplateValidator... validators) {
    if (validators == null) {
      return;
    }
    Collections.addAll(this.workbookTemplateValidators, validators);
  }

  @Override
  public void addSheetTemplateValidator(SheetTemplateValidator... validators) {
    if (validators == null) {
      return;
    }
    Collections.addAll(this.sheetTemplateValidators, validators);
  }

  @Override
  public void addWorkbookValidator(WorkbookValidator... validators) {
    if (validators == null) {
      return;
    }
    Collections.addAll(this.workbookValidators, validators);
  }

  @Override
  public void addSheetValidator(SheetValidator... validators) {
    if (validators == null) {
      return;
    }
    Collections.addAll(this.sheetValidators, validators);
  }

  @Override
  public void addRowValidator(RowValidator... validators) {
    if (validators == null) {
      return;
    }
    Collections.addAll(this.rowValidators, validators);
  }

  @Override
  public void addCellValidator(CellValidator... validators) {
    if (validators == null) {
      return;
    }
    Collections.addAll(this.cellValidators, validators);
  }

  @Override
  public List<TemplateValidateMessage> getTemplateErrorMessages() {
    return templateErrorMessages;
  }

  @Override
  public List<DataValidateMessage> getDataErrorMessages() {
    return dataErrorMessages;
  }

  @Override
  public boolean validate() {
    if (excelWorkbook == null) {
      throw new ExcelReadException("workbook is null");
    }

    // check dependency
    checkValidatorKeyDependency();

    // valid template
    validWorkbookTemplate(excelWorkbook);
    if (CollectionUtils.isNotEmpty(templateErrorMessages)) {
      return false;
    }

    for (ExcelSheet excelSheet : excelWorkbook.getSheets()) {
      validSheetTemplate(excelSheet);

      if (CollectionUtils.isNotEmpty(templateErrorMessages)) {
        return false;
      }
    }

    // valid data
    validWorkbook(excelWorkbook);

    for (ExcelSheet excelSheet : excelWorkbook.getSheets()) {

      validSheet(excelSheet);

      for (ExcelRow excelRow : excelSheet.getRows()) {

        validRowCells(excelRow);
      }
    }


    return CollectionUtils.isEmpty(dataErrorMessages);
  }

  /**
   * check if dependency correct
   */
  private void checkValidatorKeyDependency() {

    Map<String, Set<String>> dependsOnHierarchy = new HashMap<>();

    dependsOnHierarchy.putAll(buildDependsOnHierarchy(rowValidators));
    dependsOnHierarchy.putAll(buildDependsOnHierarchy(cellValidators));

    Set<String> satisfiedKeys = new HashSet<>();
    for (String key : dependsOnHierarchy.keySet()) {
      Set<String> dependencyKeys = new HashSet<>();
      checkValidatorKeyDependencyHierarchy(dependsOnHierarchy, satisfiedKeys, dependencyKeys, key);
      satisfiedKeys.addAll(dependencyKeys);
    }
  }

  private void checkValidatorKeyDependencyHierarchy(
      Map<String, Set<String>> dependsOnHierarchy,
      Set<String> satisfiedKeys,
      Set<String> dependencyKeys,
      String key
  ) {

    if (satisfiedKeys.contains(key)) {
      return;
    }

    dependencyKeys.add(key);
    for (String dependsOn : dependsOnHierarchy.get(key)) {

      if (!dependsOnHierarchy.containsKey(dependsOn)) {
        throw new IllegalArgumentException("dependency missing key [" + dependsOn + "]");
      }

      if (dependencyKeys.contains(dependsOn)) {
        throw new IllegalArgumentException("dependency cycling on [" + key + "] and [" + dependsOn + "]");
      }

      checkValidatorKeyDependencyHierarchy(dependsOnHierarchy, satisfiedKeys, dependencyKeys, dependsOn);
    }
  }

  /*=========================
   below is internal validate
   ==========================*/
  private void validWorkbookTemplate(ExcelWorkbook excelWorkbook) {

    for (WorkbookTemplateValidator validator : workbookTemplateValidators) {

      if (!validator.validate(excelWorkbook)) {

        templateErrorMessages.add(validator.getErrorMessage());

      }

    }
  }

  private void validSheetTemplate(ExcelSheetTemplate excelSheet) {

    for (SheetTemplateValidator validator : sheetTemplateValidators) {

      if (!validator.validate(excelSheet)) {

        templateErrorMessages.add(validator.getErrorMessage());
      }
    }
  }

  private void validWorkbook(ExcelWorkbook excelWorkbook) {

    for (WorkbookValidator validator : workbookValidators) {

      DataValidateMessage result = validator.validate(excelWorkbook);
      if (!ValidateResult.SUCCESS.equals(result.getResult())) {

        dataErrorMessages.add(result);
      }
    }

  }

  private void validSheet(ExcelSheet excelSheet) {

    for (SheetValidator validator : sheetValidators) {

      DataValidateMessage result = validator.validate(excelSheet);
      if (!ValidateResult.SUCCESS.equals(result.getResult())) {

        dataErrorMessages.add(result);
      }
    }

  }

  private void validRowCells(ExcelRow excelRow) {

    Map<String, Set<String>> dependsOnHierarchy = new HashMap<>();

    dependsOnHierarchy.putAll(buildDependsOnHierarchy(rowValidators));
    dependsOnHierarchy.putAll(buildDependsOnHierarchy(cellValidators));

    Map<String, List<DataValidator>> validatorMap = new HashMap<>();

    validatorMap.putAll(buildValidatorMap(rowValidators));
    validatorMap.putAll(buildValidatorMap(cellValidators));

    Map<String, Set<ValidateResult>> result = new HashMap<>();

    for (RowValidator validator : rowValidators) {
      result.putAll(validRowCellsHierarchy(validatorMap, result, dependsOnHierarchy, excelRow, validator.getKey()));
    }
    for (CellValidator validator : cellValidators) {
      result.putAll(validRowCellsHierarchy(validatorMap, result, dependsOnHierarchy, excelRow, validator.getKey()));
    }

  }

  private Map<String, Set<ValidateResult>> validRowCellsHierarchy(
      Map<String, List<DataValidator>> validatorMap,
      Map<String, Set<ValidateResult>> allResult,
      Map<String, Set<String>> dependsOnHierarchy,
      ExcelRow excelRow,
      String key
  ) {
    Map<String, Set<ValidateResult>> result = new HashMap<>();

    if (allResult.containsKey(key)) {
      result.put(key, allResult.get(key));
      return result;
    }

    Set<String> dependsOns = dependsOnHierarchy.get(key);

    if (CollectionUtils.isNotEmpty(dependsOns)) {

      for (String dependsOn : dependsOns) {

        result.putAll(validRowCellsHierarchy(validatorMap, result, dependsOnHierarchy, excelRow, dependsOn));
      }

    }

    boolean skip = ifSkip(result);

    Set<ValidateResult> vrs = new HashSet<>();
    for (DataValidator dataValidator : validatorMap.get(key)) {

      if (skip) {

        vrs.add(ValidateResult.SKIP);
      } else {

        vrs.add(doRowCellsValid(dataValidator, excelRow));
      }
    }
    result.put(key, vrs);

    return result;
  }

  private boolean ifSkip(Map<String, Set<ValidateResult>> result) {

    Set<ValidateResult> dependsOnVrs = new HashSet<>();
    for (Set<ValidateResult> vr : result.values()) {
      dependsOnVrs.addAll(vr);
    }

    return dependsOnVrs.contains(ValidateResult.FAILURE) || dependsOnVrs.contains(ValidateResult.SKIP);
  }

  private ValidateResult doRowCellsValid(DataValidator dataValidator, ExcelRow excelRow) {

    if (dataValidator instanceof RowValidator) {

      return ((RowValidator) dataValidator).validate(excelRow);
    } else {

      return ((CellValidator) dataValidator).validate(excelRow)
    }
  }

  private <VALIDATOR extends DataValidator> Map<String, Set<String>> buildDependsOnHierarchy(List<VALIDATOR> dataValidators) {
    Map<String, Set<String>> dependsOnHierarchy = new HashMap<>();

    for (VALIDATOR validator : dataValidators) {
      String key = validator.getKey();
      Set<String> dependsOn = validator.getDependsOn();

      if (!dependsOnHierarchy.containsKey(key)) {
        dependsOnHierarchy.put(key, dependsOn);
      }
      dependsOnHierarchy.get(key).addAll(dependsOn);
    }

    return dependsOnHierarchy;
  }

  private <VALIDATOR extends DataValidator> Map<String, List<DataValidator>> buildValidatorMap(List<VALIDATOR> validators) {

    Map<String, List<DataValidator>> validatorMap = new HashMap<>();

    for (VALIDATOR validator : validators) {
      String key = validator.getKey();
      if (!validatorMap.containsKey(key)) {
        validatorMap.put(key, new ArrayList<>());
      }
      validatorMap.get(key).add(validator);
    }

    return validatorMap;
  }

}
