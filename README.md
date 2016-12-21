# java-excel-tools

动态的Excel数据与Java Bean相互转换的工具，Excel中每一行对应一个Java Bean，每一列对应Java Bean中的一个字段，并且提供将Excel数据转换成Java Bean时提供校验，将校验信息以备注形式写到原始文件上

## API
	
1. <code>interface UserFileTemplate</code>

	模板工厂，用来得到<code>UserFileValidator</code>和<code>UserFileImporter</code>
	
1. <code>interface UserFileValidator</code>

	文件校验器，用来校验Excel中所填数据的正确性	
	
1. <code>interface UserFileImporter</code>

	文件导入器，用来把Excel中所填数据转换成Java Bean
	
1. <code>interface ModelFactory</code>

	对象生成工厂，用来产生导入数据对应的Java Bean
	
1. <code>interface ValueSetter</code>

	对象字段赋值器，用来将Excel中所填数据赋值到Java Bean对应字段
	
1. <code>interface DataProcessor</code>

	数据处理器，用来处理Excel数据转成的Java Bean集合
	
1.	<code>interface UserFileGenerator</code>

	文件生成器，用来生成Excel文件
	
1. <code>interface CellPrompter</code>

	字段提示生成器，用来生成在导入时所填数据的提示
	
1. <code>interface CellValueExtractor</code>

	对象字段提取器，用来把Java Bean字段转换成可读文字写到Excel中
	
## Excel导入模板格式
- 第一行标题
- 第二行对应Java Bean的字段
- 第三行提示
- 第四行以后是数据	

## pom.xml
```
<repositories>
  <repository>
    <id>java-excel-tools-mvn-repository</id>
    <url>https://raw.github.com/xfslove/mvn-repository/mvn-repository/</url>
  </repository>
</repositories>

... others

<dependency>
  <groupId>java.excel.tools</groupId>
  <artifactId>java-excel-tools</artifactId>
  <version>1.1.0</version>
</dependency>
```


### Quick Start
- 如下Excel文件

<table>
<tr><td>姓名</td><td>年龄</td><td>生日</td></tr>
<td>person.name</td><td>person.age</td><td>person.birthday</td>
<tr><td>必填，姓名</td><td>必填，数字</td><td>必填，yyyy-MM-dd</td></tr>
<tr><td>Scarlett</td><td>18</td><td>1984-11-22</td></tr>
<tr><td>...</td><td>...</td><td>...</td></tr>
</table>

- 构建一个Excel模板

```
UserFileTemplate excelTemplate = new ExcelFileTemplate(excel);
```

- 生成导入文件

```
userFileGenerator = new ExcelFileGenerator();
userFileGenerator.setTitles("姓名", "年龄", "生日");
userFileGenerator.setFields("person.name", "person.age", "person.birthday");
// 添加字段提示
userFileGenerator.addCellPrompters(
        new PromptBuilder()
            .prompt("person.age", "整数")
            .prompt("person.birthday", "yyyy-MM-dd")
            .add(new RequiredPrompter("person.name"))
            .build()
    );
userFileGenerator.generate(excel);
```

- 校验

```
excelTemplate.getUserFileValidator();
// 添加校验器
userFileValidator.addCellValidator(
		new RequiredValidator("person.name"),
	  	new LocalDateValidator("person.birthday", "yyyy-MM-dd"),
     	new IntValidator("person.age")
    );
// 得到校验结果
boolean passed = userFileValidator.validate();
// 如果验证失败，把错误信息写到文件上
if (!passed) {
	userFileValidator.writeFailureMessageComments();
}  
```

- Java Bean赋值

```
excelTemplate.getUserFileImporter();
// 添加赋值器
userFileImporter.addCellValueSetter(
        new LocalDateValueSetter("person.birthday", "yyyy-MM-dd")
    );
// 设置Java Bean对象工厂
userFileImporter.setModelFactory(new PersonModelFactory());
// 设置转换后的对象处理器
userFileImporter.process(new PersonListProcessor());
```

- 导出数据

	和生成导入文件类似，只需要把导出的数据给<code>ExcelFileGenerator</code>即可

```
userFileGenerator.setData(data);
```
