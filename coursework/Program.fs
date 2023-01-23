let (^) a b = a b

module Manifest =
    type СompatibilityTag = 
        Color | Underline | Strikethrough | Italic
    type Attribute = { 
        id: string; compatibilityTag: СompatibilityTag; domain: string }
    type PackageManifest = { 
        id: string; attributes: Attribute list }

open Manifest

type Fragment = Character of int | Range of int * int 
type FragmentHighlited = Fragment -> Attribute -> unit

module HierarchicalAnalysis =
    type HighlitningData = list<Fragment * Attribute * string>
    type Alternative = {
        domain: string
        packageId: string
        attributeId: string
        compatibilityTag: СompatibilityTag 
    }

    type CriteriaLevel = Domain | Package | Attribute

    type Criteria = { criteriaLevel: CriteriaLevel; name: string; wieght: float;  }

    let buildHierarchy highlitningData =
        let allAlternatives (hd: HighlitningData) =
            hd |> List.map (fun (_, attr, packageId) -> { 
                attributeId = attr.id
                packageId = packageId
                domain = attr.domain
                compatibilityTag = attr.compatibilityTag 
            }) |> List.distinct

        let allAlternatives = allAlternatives highlitningData

        let rec findUniqueEntries (domains, packages, attributes) = function
            | [] -> (domains, packages, attributes)
            | alternative::tail -> 
                findUniqueEntries 
                    ((Set.add alternative.domain domains),
                     (Set.add alternative.packageId packages), 
                     (Set.add alternative.attributeId attributes))
                    tail
                    

        let (domains, packages, attributes) = findUniqueEntries (Set.empty, Set.empty, Set.empty) allAlternatives
        
        let buildLayer entries criteriaLevel =
            entries
            |> Seq.map ^ fun entry -> { 
                criteriaLevel = criteriaLevel
                name = entry
                wieght = 1. / float (Set.count entries) 
            }
            |> List.ofSeq

        let domains = buildLayer domains Domain

        let packages = buildLayer packages Package

        let attributes = buildLayer attributes Attribute
        
        [domains; packages; attributes]

    let evaluateHierarchy layers package =
        let getAffectedCriterias criteria : Criteria list =
            failwith "Not implemented"

        for layer in layers do
            for criteria in layer do
                let affected = getAffectedCriterias criteria

        ()

()